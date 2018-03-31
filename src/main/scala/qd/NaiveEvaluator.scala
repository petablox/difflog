package qd

case class NaiveEvaluator(program: Program) extends Evaluator(program) {

  override def apply(edb: Config): Config = {
    var config = Config(allRelations.map(schema => schema -> edb.getOrElse(schema, Instance(schema))).toMap)
    var oldConfig = Config()
    while (config.numTuples > oldConfig.numTuples) {
      oldConfig = config
      for (rule <- rules) {
        config = immediateConsequence(rule, config)
      }
      assert(config.numTuples >= oldConfig.numTuples)
    }
    config
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
