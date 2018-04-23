package qd

case class SeminaiveEvaluator(override val program: Program) extends Evaluator("Seminaive", program) {

  override def apply(edb: Config): Config = {
    var config: Config = edb
    var delta = config
    while (delta.totalWeight > 0) {
      /* println(s"config.totalWeight: ${config.totalWeight}. " +
              s"delta.totalWeight: ${delta.totalWeight}.") */
      val (newConfig, newDelta) = immediateConsequence(config, delta)
      assert(newConfig.numTuples >= config.numTuples)
      config = newConfig
      delta = newDelta
    }
    config
  }

  def immediateConsequence(config: Config, delta: Config): (Config, Config) = {
    var newConfig = config
    var newDelta = Config.EMPTY
    for (rule <- rules) {
      val (instance, instDelta) = immediateConsequence(rule, config, delta)
      val relation = rule.head.relation
      newConfig = newConfig + (relation -> (newConfig(relation) ++ instance))
      newDelta = newDelta + (relation -> (newDelta(relation) ++ instDelta))
    }
    (newConfig, newDelta)
  }

  def immediateConsequence(rule: Rule, config: Config, delta: Config): (Instance, Instance) = {
    val relation = rule.head.relation
    val oldInstance = config(relation)

    var newInstance = oldInstance
    var newDelta = Instance(relation)
    for (literal <- rule.body) {
      val literalDelta = immediateConsequence(rule, literal, config, delta)
      newInstance = newInstance ++ literalDelta
      newDelta = newDelta ++ literalDelta
    }

    (newInstance, newDelta -- oldInstance)
  }

  def immediateConsequence(rule: Rule, deltaLiteral: Literal, config: Config, delta: Config): Instance = {
    require(rule.body.contains(deltaLiteral))

    var bodyVals = Set(Valuation())
    for (literal <- rule.body) {
      bodyVals = if (literal == deltaLiteral) extend(literal, delta, bodyVals)
                 else extend(literal, config, bodyVals)
    }
    val newTuples = bodyVals.flatMap(rule.head.concretize).toMap

    val newInstance = config(rule.head.relation) ++ newTuples
    newInstance
  }

  def extend(literal: Literal, config: Config, bodyVals: Set[Valuation]): Set[Valuation] = {
    for (valuation <- bodyVals;
         tv <- config(literal.relation);
         (tuple, score) = tv;
         newValuation <- extend(literal, tuple, valuation))
      yield newValuation * score * literal.coeff
  }

  def extend(literal: Literal, tuple: DTuple, valuation: Valuation): Option[Valuation] = {
    var ans = valuation
    for ((par, field) <- literal.parameters.zip(tuple)) {
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
