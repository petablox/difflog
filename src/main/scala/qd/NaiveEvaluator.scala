package qd

case class NaiveEvaluator(program: Program) extends Evaluator(program) {

  override def apply(edb: Config): Config = {
    var config = Config(allRelations.map(schema => schema -> edb.getOrElse(schema, Instance(schema))).toMap)
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
    var bodyVals = Set(Valuation())
    for (literal <- rule.body) {
      bodyVals = extend(literal, config, bodyVals)
    }
    val newTuples = bodyVals.flatMap(rule.head.concretize)

    val newInstance = config(rule.head.relation) ++ newTuples
    val newConfig = config + (rule.head.relation -> newInstance)
    val changed = newInstance.numTuples > config(rule.head.relation).numTuples
    (newConfig, changed)
  }

  def extend(literal: Literal, config: Config, bodyVals: Set[Valuation]): Set[Valuation] = {
    for (valuation <- bodyVals;
         tuple <- config(literal.relation).tuples;
         newValuation <- extend(literal, tuple, valuation))
    yield newValuation
  }

  def extend(literal: Literal, tuple: DTuple, valuation: Valuation): Option[Valuation] = {
    var ans = valuation
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

}
