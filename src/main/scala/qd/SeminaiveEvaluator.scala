package qd

import java.time.LocalTime

import scala.collection.parallel.ParSet

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
    var newConfig = config
    var newDelta = Config()
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

    var bodyVals = ParSet(Valuation())
    for (literal <- rule.body) {
      bodyVals = if (literal == deltaLiteral) extend(literal, delta, bodyVals)
                 else extend(literal, config, bodyVals)
    }
    val newTuples = bodyVals.map(_ * rule.coeff).flatMap(rule.head.concretize).toMap.seq

    val newInstance = config(rule.head.relation) ++ newTuples
    newInstance
  }

  def extend(literal: Literal, config: Config, bodyVals: ParSet[Valuation]): ParSet[Valuation] = {
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
