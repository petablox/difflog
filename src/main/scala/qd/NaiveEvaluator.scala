package qd

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Naive Datalog Evaluator

case class NaiveEvaluator(program: Program) extends Evaluator(program) {

  override def eval(edb: Config): Config = {
    var config = Config(allSchemas.map(schema => schema -> edb.getOrElse(schema, Instance(schema))).toMap)
    var done = false
    while (!done) {
      done = true
      for (rule <- rules) {
        val (configPrime, changed) = immediateConsequence(rule, config)
        if (changed) {
          config = configPrime
          done = false
        }
      }
    }
    config
  }

  // Applies a rule to a configuration. Returns the new configuration and signals whether anything was changed
  // (_, true) iff something was changed.
  def immediateConsequence(rule: Rule, config: Config): (Config, Boolean) = {
    val (relPrime, changed) = immediateConsequenceImpl(rule, config)
    (config + (rule.head.relation -> relPrime), changed)
  }

  // The function immediateConsequence maps configurations to successor configurations
  // The following function maps the current configuration to the updated value of just the relation on the head
  def immediateConsequenceImpl(rule: Rule, config: Config): (Instance, Boolean) = {
    var bodyVals: Set[Valuation] = Set()
    for (literal <- rule.body) {
      bodyVals = extend(literal, config, bodyVals)
    }
    val relPrime = config(rule.head.relation) ++ concretize(rule.head, bodyVals)
    (relPrime, relPrime.numTuples > config(rule.head.relation).numTuples)
  }

  def extend(literal: Literal, config: Config, bodyVals: Set[Valuation]): Set[Valuation] = {
    def extendVT(valuation: Valuation, tuple: DTuple): Option[Valuation] = {
      var ans: Valuation = valuation
      for ((par, field) <- literal.parameters.zip(tuple.fields)) {
        par match {
          case v @ Variable(_, _) =>
            if (!ans.contains(v)) ans = ans + (v -> field)
            else if (ans(v) != field) return None
          case Constant(c, _) => if (c != field) return None
        }
      }
      Some(ans)
    }
    def extend(valuation: Valuation): Set[Valuation] = config(literal.relation).tuples.flatMap(tuple => extendVT(valuation, tuple))
    bodyVals.flatMap(extend)
  }

  def concretize(head: Literal, bodyVals: Set[Valuation]): Instance = {
    def concretize(valuation: Valuation): Set[Valuation] = {
      var ans = Set(valuation)

      for (v <- head.parameters.filter(_.isInstanceOf[Variable])
                               .map(_.asInstanceOf[Variable])) {
        ans = ans.flatMap(valPrime => {
          if (!valPrime.contains(v)) v.domain.allAtoms.map(atom => valPrime + (v -> atom))
          else Set(valPrime)
        })
      }

      ans
    }

    def val2Tuple(valuation: Valuation): DTuple = {
      val fields = head.parameters.map {
        case v @ Variable(_, _) => valuation(v)
        case Constant(c, _) => c
      }
      DTuple(fields:_*)
    }

    Instance(head.relation, bodyVals.flatMap(concretize).map(val2Tuple))
  }

}

object NaiveEvaluator {
  def apply(program: Program, edb: Config): Config = NaiveEvaluator(program).eval(edb)
}
