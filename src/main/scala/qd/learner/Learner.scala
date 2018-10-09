package qd
package learner

import qd.evaluator.Evaluator
import qd.instance.Config
import qd.problem.Problem

class Learner(
               val q0: Problem,
               val evaluator: Evaluator,
               val scorerFactory: (Config[FValue], Config[FValue], Evaluator) => Scorer
             ) {

  val edb: Config[FValue] = q0.edb
  val referenceIDB: Config[FValue] = q0.idb

  val tokens: Set[Token] = q0.pos.keySet
  val scorer: Scorer = scorerFactory(edb, referenceIDB, evaluator)

  private var pos: TokenVec = q0.pos
  private var rules: Set[Rule[FValue]] = q0.rules
  private var currentIDB: Config[FValue] = evaluator(rules, edb)
  private var bestIteration: Option[(TokenVec, Set[Rule[FValue]], Config[FValue], Double)] = None
  private var step: TokenVec = TokenVec(tokens.map(token => token -> 1.0).toMap)

  def getPos: TokenVec = pos
  def getRules: Set[Rule[FValue]] = rules
  def getCurrentIDB: Config[FValue] = currentIDB
  def getBestIteration: (TokenVec, Set[Rule[FValue]], Config[FValue], Double) = bestIteration.get

  def learn(tgtLoss: Double, maxIters: Int): (TokenVec, Set[Rule[FValue]], Config[FValue], Double) = {
    require(maxIters > 0)
    var (currLoss, numIters, gradAbs) = (tgtLoss + 1, 0, 1.0)
    val startTime = System.nanoTime()
    while (numIters < maxIters && currLoss >= tgtLoss && gradAbs > 0 && step.abs > 0.0) {
      update()
      currLoss = scorer.loss(currentIDB)
      numIters += 1
      gradAbs = scorer.gradientLoss(pos, currentIDB).abs
    }
    val endTime = System.nanoTime()
    val timePerIter = (endTime - startTime) / 1.0e9 / numIters
    scribe.info(s"#Iterations: $numIters")
    scribe.info(s"Time / iteration: $timePerIter seconds.")
    getBestIteration
  }

  def update(): Unit = {
    val oldPos = pos
    pos = computeNewPos()
    rules = pos(rules)
    currentIDB = evaluator(rules, edb)
    step = pos - oldPos

    val loss = scorer.loss(currentIDB)
    if (bestIteration.isEmpty || loss < bestIteration.get._4) bestIteration = Some((pos, rules, currentIDB, loss))
  }

  def computeNewPos(): TokenVec = {
    val loss = scorer.loss(currentIDB)
    val grad = scorer.gradientLoss(pos, currentIDB)

    // if (grad.abs == 0) { this }
    val delta = grad.unit * loss / grad.abs
    val newPos = pos - delta
    val newPosLim = newPos.clip(0.0, 1.0).clip(0.01, 0.99, pos)
    val newStep = newPosLim - pos // (newPosLim - pos) * 0.8 + step * 0.2
    val bestLoss = bestIteration.map(_._4).getOrElse(Double.PositiveInfinity)
    // logger.debug(s"  grad: $grad")
    scribe.info(s"  $loss, $bestLoss, ${newPos.abs}, ${grad.abs}, ${newStep.abs}")
    newPosLim
  }

  def keepHighestTokens(): Seq[(TokenVec, Set[Rule[FValue]], Config[FValue], Double)] = {
    val bestPos = bestIteration.get._1
    val sortedTokens = bestPos.toSeq.sortBy(-_._2).map(_._1)

    scribe.info("Keeping highest tokens")
    for (k <- Range(1, sortedTokens.size + 1))
    yield {
      val highestTokens = sortedTokens.take(k).toSet
      val highestRules = bestIteration.get._2.filter(_.coeff.l.toVector.toSet.subsetOf(highestTokens))

      val highestPos = TokenVec(tokens, t => if (highestTokens.contains(t)) bestPos.map(t) else 0.0)
      val highestIDB = evaluator(highestRules, edb)
      val highestError = scorer.loss(highestIDB)

      scribe.info(s"$k ${highestRules.size} $highestError")
      (highestPos, highestRules, highestIDB, highestError)
    }
  }

  def keepUseful(): Seq[(TokenVec, Set[Rule[FValue]], Config[FValue], Double)] = {
    scribe.info("Preserving useful tokens")
    for ((hipos, hirules, hiIDB, _) <- keepHighestTokens())
      yield {
        val usefulTokens = for (rel <- scorer.outputRels;
                                (_, v) <- hiIDB(rel).support;
                                token <- v.l.toVector)
                           yield token

        val usefulPos = TokenVec(tokens, t => if (usefulTokens.contains(t)) hipos.map(t) else 0.0)
        val usefulRules = usefulPos(hirules).filter(_.coeff.v > 0.0)
        val usefulIDB = evaluator(usefulRules, edb)
        val usefulError = scorer.loss(usefulIDB)

        scribe.info(s"$usefulError ${usefulRules.size}")
        (usefulPos, usefulRules, usefulIDB, usefulError)
      }
  }

  def reinterpret(): Seq[(TokenVec, Set[Rule[FValue]], Config[FValue], Double)] = {
    scribe.info("Reinterpreting program")
    for ((_, urules, _, _) <- keepUseful())
    yield {
      val utokens = urules.flatMap(_.coeff.l.toVector)
      val rpos = TokenVec(tokens, t => if (utokens.contains(t)) 1.0 else 0.0)
      val rrules = rpos(urules)
      val rIDB = evaluator(rrules, edb)
      // val rError = scorer.loss(rIDB)
      val rf1 = scorer.f1(rIDB, 0.5)

      scribe.info(s"$rf1 ${rrules.size}")
      (rpos, rrules, rIDB, rf1)
    }
  }

}
