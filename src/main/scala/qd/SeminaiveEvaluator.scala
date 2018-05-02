package qd

case class SeminaiveEvaluator(override val program: Program) extends Evaluator("Seminaive", program) {

  override def apply(edb: Config): Config = {
    var (oldConfig, config, delta) = (Config(), edb, edb)
    while (delta.nonEmptySupport) {
      // println(s"S: ${LocalTime.now}")
      val (newConfig, newDelta) = immediateConsequence(config, delta)
      oldConfig = config
      config = newConfig
      delta = newDelta
    }
    config
  }

  def immediateConsequence(config: Config, delta: Config): (Config, Config) = {
    var (newConfig, deltaCurr, deltaNext) = (config, delta, Config())
    for (rule <- rules) {
      val cdd = immediateConsequence(rule, newConfig, deltaCurr, deltaNext)
      newConfig = cdd._1
      deltaCurr = cdd._2
      deltaNext = cdd._3
    }
    (newConfig, deltaNext)
  }

  def immediateConsequence(rule: Rule, config: Config,
                           deltaCurr: Config, deltaNext: Config): (Config, Config, Config) = {
    var (newConfig, newDeltaCurr, newDeltaNext) = (config, deltaCurr, deltaNext)
    for (literal <- rule.body) {
      val cdd = immediateConsequence(rule, literal, newConfig, newDeltaCurr, newDeltaNext)
      newConfig = cdd._1
      newDeltaCurr = cdd._2
      newDeltaNext = cdd._3
    }

    (newConfig, newDeltaCurr, newDeltaNext)
  }

  def immediateConsequence(rule: Rule, deltaLiteral: Literal,
                           config: Config, deltaCurr: Config, deltaNext: Config): (Config, Config, Config) = {
    require(rule.body.contains(deltaLiteral))

    var bodyVals = Set(Valuation())
    for (literal <- rule.body) {
      bodyVals = if (literal == deltaLiteral) extend(literal, deltaCurr, bodyVals)
                 else extend(literal, config, bodyVals)
    }
    val newTuples = bodyVals.map(_ * rule.coeff).flatMap(rule.head.concretize).toMap

    val relation = rule.head.relation
    val oldInstance = config(relation)
    val newInstance = newTuples.foldLeft(oldInstance)(_ + _)
    val newConfig = config + (relation -> newInstance)

    val deltaTuples = newTuples.filter { case (tuple, value) => value > oldInstance(tuple) }

    val dcr = deltaCurr(relation)
    val ndcr = deltaTuples.foldLeft(dcr)(_ + _)
    val newDeltaCurr = deltaCurr + (relation -> ndcr)

    val dnr = deltaNext(relation)
    val ndnr = deltaTuples.foldLeft(dnr)(_ + _)
    val newDeltaNext = deltaNext + (relation -> ndnr)

    (newConfig, newDeltaCurr, newDeltaNext)
  }

  def extend(literal: Literal, config: Config, bodyVals: Set[Valuation]): Set[Valuation] = {
    for (valuation <- bodyVals;
         f = valuation.filter(literal);
         tv <- config(literal.relation).filter(f).support;
         (tuple, score) = tv;
         newValuation <- extend(literal, tuple, valuation))
    yield newValuation * (literal.coeff + score)
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
