package qd

import java.time.LocalTime

case class NaiveEvaluator(override val program: Program) extends Evaluator("Naive", program) {

  override def apply(edb: Config): Config = {
    var (config, changed) = (edb, true)
    while (changed) {
      println(s"N: ${LocalTime.now}")
      val cd = immediateConsequence(config)
      config = cd._1
      changed = cd._2
    }
    config
  }

  def immediateConsequence(config: Config): (Config, Boolean) = {
    var (c, d) = (config, false)
    for (rule <- rules) {
      val (cPrime, id) = immediateConsequence(rule, c)
      c = cPrime
      d = d || id
    }
    (c, d)
  }

  // Applies a rule to a configuration
  def immediateConsequence(rule: Rule, config: Config): (Config, Boolean) = {
    val relation = rule.head.relation
    var bodyVals = Set(Valuation())
    for (literal <- rule.body) {
      bodyVals = extend(literal, config, bodyVals)
    }
    val newTuples = bodyVals.map(_ * rule.coeff).flatMap(rule.head.concretize).toMap

    val oldInstance = config(relation)
    val newInstance = newTuples.foldLeft(oldInstance)(_ + _)
    val newConfig = config + (relation -> newInstance)

    val changed = newTuples.exists { case (tuple, value) => value > oldInstance(tuple) }

    (newConfig, changed)
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
