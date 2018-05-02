package qd

import java.time.LocalTime

import scala.collection.parallel.ParSet

case class NaiveEvaluator(override val program: Program) extends Evaluator("Naive", program) {

  override def apply(edb: Config): Config = {
    var (oldConfig, config, delta) = (Config(), edb, edb)
    while (delta.nonEmptySupport) {
      // println(s"N: ${LocalTime.now}")
      val cd = immediateConsequence(config)
      oldConfig = config
      config = cd._1
      delta = cd._2
    }
    config
  }

  def immediateConsequence(config: Config): (Config, Config) = {
    var (c, d) = (config, Config())
    for (rule <- rules) {
      val relation = rule.head.relation
      val (cPrime, instDelta) = immediateConsequence(rule, c)
      c = cPrime
      d = d + (relation -> (d(relation) ++ instDelta))
    }
    (c, d)
  }

  // Applies a rule to a configuration
  def immediateConsequence(rule: Rule, config: Config): (Config, Instance) = {
    val relation = rule.head.relation
    var bodyVals = ParSet(Valuation())
    for (literal <- rule.body) {
      bodyVals = extend(literal, config, bodyVals)
    }
    val newTuples = bodyVals.map(_ * rule.coeff).flatMap(rule.head.concretize).toMap.seq

    val oldInstance = config(relation)
    val newInstance = oldInstance ++ newTuples
    val newConfig = config + (relation -> newInstance)
    (newConfig, newTuples.foldLeft(Instance(relation))(_ + _) -- oldInstance)
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
