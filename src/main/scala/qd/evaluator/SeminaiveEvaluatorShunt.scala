package qd
package evaluator

import scala.collection.parallel.ParSeq

case class SeminaiveEvaluatorShunt[T <: Value[T]](program: Program[T])(implicit vs: Semiring[T]) extends EvaluatorShunt[T] {

  override val name: String = s"SeminaiveEvaluator(${program.name})"

  override def apply(edb: Config[T]): Config[T] = {
    var (oldConfig, config, delta) = (Config(), edb, edb)
    while (delta.nonEmptySupport) {
      val (newConfig, newDelta) = immediateConsequence(config, delta)
      oldConfig = config
      config = newConfig
      delta = newDelta
    }
    config
  }

  var totalTime = 0l
  var numImmediateConsequence = 0

  def immediateConsequence(config: Config[T], delta: Config[T]): (Config[T], Config[T]) = {
    var (newConfig, deltaCurr, deltaNext) = (config, delta, Config())
    for (rule <- rules) {
      val cdd = immediateConsequence(rule, newConfig, deltaCurr, deltaNext)
      newConfig = cdd._1
      deltaCurr = cdd._2
      deltaNext = cdd._3
    }
    (newConfig, deltaNext)
  }

  def immediateConsequence(rule: Rule[T], config: Config[T],
                           deltaCurr: Config[T], deltaNext: Config[T]): (Config[T], Config[T], Config[T]) = {
    var (newConfig, newDeltaCurr, newDeltaNext) = (config, deltaCurr, deltaNext)
    for (literal <- rule.body) {
      val cdd = immediateConsequence(rule, literal, newConfig, newDeltaCurr, newDeltaNext)
      newConfig = cdd._1
      newDeltaCurr = cdd._2
      newDeltaNext = cdd._3
    }

    (newConfig, newDeltaCurr, newDeltaNext)
  }

  def immediateConsequence(rule: Rule[T], deltaLiteral: Literal, config: Config[T],
                           deltaCurr: Config[T], deltaNext: Config[T]): (Config[T], Config[T], Config[T]) = {
    require(rule.body.contains(deltaLiteral))

    var assignments = ParSeq(Assignment.Empty)
    var remainingLits = rule.body
    for (literal <- rule.body) {
      assignments = if (literal == deltaLiteral) extend(literal, deltaCurr, assignments)
                 else extend(literal, config, assignments)

      remainingLits = remainingLits - literal
      if (assignments.size > 100) {
        val relevantVars = remainingLits.foldLeft(rule.head.variables)(_ ++ _.variables)

        assignments = assignments.map(_.project(relevantVars))
        assignments = assignments.groupBy(_.map)
                                 .mapValues(_.map(_.score).foldLeft(vs.Zero)(_ + _))
                                 .toSeq.map(mv => Assignment(mv._1, mv._2))
      }
    }
    val newTuples = assignments.map(_ * rule.coeff).map(_.toTuple(rule.head)).toMap

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

  def extend(literal: Literal, config: Config[T], bodyVals: ParSeq[Assignment[T]]): ParSeq[Assignment[T]] = {
    for (assignment <- bodyVals;
         f = assignment.toFilter(literal);
         (tuple, score) <- config(literal.relation).filter(f);
         newValuation <- extend(literal, tuple, assignment))
    yield newValuation * score
  }

  def extend(literal: Literal, tuple: DTuple, assignment: Assignment[T]): Option[Assignment[T]] = {
    var ans = assignment
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
