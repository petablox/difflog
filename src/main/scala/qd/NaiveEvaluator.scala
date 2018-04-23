package qd

case class NaiveEvaluator(override val program: Program) extends Evaluator("Naive", program) {

  override def apply(edb: Config): Config = {
    var oldConfig = Config()
    var config = edb
    while (config.totalWeight > oldConfig.totalWeight) {
      // println(s"N config.numTuples: ${config.numTuples}")
      oldConfig = config
      config = immediateConsequence(oldConfig)
      assert(config.totalWeight >= oldConfig.totalWeight)
    }
    config
  }

  def immediateConsequence(config: Config): Config = {
    var ans = config
    for (rule <- rules) {
      val newAns = immediateConsequence(rule, ans)
      assert(newAns.totalWeight >= ans.totalWeight)
      ans = newAns
    }
    ans
  }

  // Applies a rule to a configuration
  def immediateConsequence(rule: Rule, config: Config): Config = {
    var bodyVals = Set(Valuation() * rule.coeff)
    for (literal <- rule.body) {
      bodyVals = extend(literal, config, bodyVals)
    }
    val newTuples = bodyVals.flatMap(rule.head.concretize).toMap

    val newInstance = config(rule.head.relation) ++ newTuples
    val newConfig = config + (rule.head.relation -> newInstance)
    newConfig
  }

  def extend(literal: Literal, config: Config, bodyVals: Set[Valuation]): Set[Valuation] = {
    for (valuation <- bodyVals;
         tv <- config(literal.relation);
         (tuple, score) = tv;
         newValuation <- extend(literal, tuple, valuation))
    yield newValuation * Instance.merge(score, literal.coeff)
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
