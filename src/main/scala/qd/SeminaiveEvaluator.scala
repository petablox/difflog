package qd

import scala.collection.parallel.ParSeq

case class SeminaiveEvaluator(override val program: Program) extends Evaluator("Seminaive", program) {

  private var numAppliedRules = 0
  private var numIters = 0

  override def apply(edb: Config): Config = {
    var (oldConfig, config, delta) = (Config(), edb, edb)
    while (delta.nonEmptySupport) {
      // println("Starting immediate consequence epoch!")
      val (newConfig, newDelta) = immediateConsequence(config, delta)
      oldConfig = config
      config = newConfig
      delta = newDelta
      numIters += 1
    }
    config
  }

  def immediateConsequence(config: Config, delta: Config): (Config, Config) = {
    var (newConfig, deltaCurr, deltaNext) = (config, delta, Config())
    numAppliedRules = 0
    for (rule <- rules) {
      // println(s"  $numAppliedRules. Evaluating rule ${rule.name}")
      // val startTime = System.nanoTime()
      // val supportSizeOrig = newConfig(rule.head.relation).support.size

      val cdd = immediateConsequence(rule, newConfig, deltaCurr, deltaNext)
      newConfig = cdd._1
      deltaCurr = cdd._2
      deltaNext = cdd._3

      // val endTime = System.nanoTime()
      // val supportSizeFinal = newConfig(rule.head.relation).support.size
      // val numFreeVars = rule.freeVariables.size
      // val numPossibleVals = rule.freeVariables.toSeq.map(_.domain.size).product
      /* println(s"  Done! Rule ${rule.name}. Relation ${rule.head.relation.name}. " +
              s"Support size original: $supportSizeOrig. Support size final: $supportSizeFinal. " +
              s"numFreeVars: $numFreeVars. numPossibleVals: $numPossibleVals. " +
              s"numIters: $numIters. Time: ${(endTime - startTime) / 1.0e9} s.") */
      numAppliedRules += 1
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
    // println(s"    deltaLiteral: $deltaLiteral")

    var bodyVals = ParSeq(Valuation.Empty)
    var remainingLits = rule.body
    for (literal <- rule.body) {
      bodyVals = if (literal == deltaLiteral) extend(literal, deltaCurr, bodyVals)
                 else extend(literal, config, bodyVals)

      remainingLits = remainingLits - literal
      if (bodyVals.size > 1000) {
        // val originalSize = bodyVals.size
        val relevantVars = remainingLits.map(_.freeVariables).foldLeft(rule.head.freeVariables)(_ ++ _)
        bodyVals = bodyVals.map(_.project(relevantVars))
        bodyVals = bodyVals.groupBy(_.backingMap)
                           .mapValues(_.map(_.score).foldLeft(Zero: Value)(_ + _))
                           .toSeq.map(mv => Valuation(mv._1, mv._2))
        // println(s"  bodyVals.size: ${bodyVals.size}. Was originally $originalSize. " +
        //         s"relevantVars: ${relevantVars.mkString(", ")}")
      }
    }
    val newTuples = bodyVals.map(_ * rule.coeff).flatMap(rule.head.concretize).toMap
    // println(s"  newTuples.size: ${newTuples.size}")

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

  def extend(literal: Literal, config: Config, bodyVals: ParSeq[Valuation]): ParSeq[Valuation] = {
    for (valuation <- bodyVals;
         f = valuation.toFilter(literal);
         (tuple, score) <- config(literal.relation).filter(f);
         newValuation <- extend(literal, tuple, valuation))
    yield newValuation * score
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
