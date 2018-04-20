package qd

case class NaiveEvaluator(program: Program) extends Evaluator(program) {

  override def apply(edb: Config): Config = {
    var oldConfig = Config()
    var config = edb.withDefault(relation => Instance(relation))
    while (config.numTuples > oldConfig.numTuples) {
      oldConfig = config
      config = immediateConsequence(oldConfig)
      assert(config.numTuples >= oldConfig.numTuples)
    }
    config
  }

  def immediateConsequence(config: Config): Config = {
    var ans = config
    for (rule <- rules) {
      val newAns = immediateConsequence(rule, ans)
      assert(newAns.numTuples >= ans.numTuples)
      ans = newAns
    }
    ans
  }

  // Applies a rule to a configuration
  def immediateConsequence(rule: Rule, config: Config): Config = {
    var bodyVals = Set(Valuation())
    for (literal <- rule.body) {
      bodyVals = extend(literal, config, bodyVals)
    }
    val newTuples = bodyVals.flatMap(rule.head.concretize)

    val newInstance = config(rule.head.relation) ++ newTuples
    val newConfig = config + (rule.head.relation -> newInstance)
    newConfig
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
