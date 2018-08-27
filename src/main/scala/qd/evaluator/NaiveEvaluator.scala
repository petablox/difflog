package qd
package evaluator

import scala.collection.parallel.ParSeq

case class NaiveEvaluator[T <: Value[T]](program: Program[T])(implicit vs: Semiring[T])
extends Evaluator[T]("Naive") {

  override def apply(edb: Config[T]): Config[T] = {
    var (config, changed) = (edb, true)
    while (changed) {
      val cd = immediateConsequence(config)
      config = cd._1
      changed = cd._2
    }
    config
  }

  def immediateConsequence(config: Config[T]): (Config[T], Boolean) = {
    var (c, d) = (config, false)
    for (rule <- rules) {
      val (cPrime, id) = immediateConsequence(rule, c)
      c = cPrime
      d = d || id
    }
    (c, d)
  }

  // Applies a rule to a configuration
  def immediateConsequence(rule: Rule[T], config: Config[T]): (Config[T], Boolean) = {
    var bodyVals = ParSeq(Assignment.Empty)
    var remainingLits = rule.body
    for (literal <- rule.body) {
      bodyVals = extend(literal, config, bodyVals)

      remainingLits = remainingLits - literal
      val relevantVars = remainingLits.map(_.variables).foldLeft(rule.head.variables)(_ ++ _)
      bodyVals = bodyVals.map(_.project(relevantVars))
      bodyVals = bodyVals.groupBy(_.map)
                         .mapValues(_.map(_.score).foldLeft(vs.Zero: T)(_ + _))
                         .toSeq.map(mv => Assignment(mv._1, mv._2))
    }
    val newTuples = bodyVals.map(_ * rule.coeff).map(_.toTuple(rule.head)).toMap

    val relation = rule.head.relation
    val oldInstance = config(relation)
    val newInstance = newTuples.foldLeft(oldInstance)(_ + _)
    val newConfig = config + (relation -> newInstance)

    val changed = newTuples.exists { case (tuple, value) => value > oldInstance(tuple) }

    (newConfig, changed)
  }

  def extend(literal: Literal, config: Config[T], bodyVals: ParSeq[Assignment[T]]): ParSeq[Assignment[T]] = {
    for (valuation <- bodyVals;
         f = valuation.toFilter(literal);
         (tuple, score) <- config(literal.relation).filter(f);
         newValuation <- extend(literal, tuple, valuation))
    yield newValuation * score
  }

  def extend(literal: Literal, tuple: DTuple, valuation: Assignment[T]): Option[Assignment[T]] = {
    var ans = valuation
    for ((par, field) <- literal.fields.zip(tuple)) {
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
