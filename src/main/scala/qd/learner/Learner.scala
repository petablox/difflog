package qd
package learner

import qd.evaluator.{Evaluator, TrieEvaluator}
import qd.problem.Problem

class Learner(q0: Problem) {

  val edb: Config[FValue] = q0.edb
  val referenceIDB: Config[FValue] = q0.idb

  val tokens: Set[Token] = q0.pos.keySet
  val evaluator: Evaluator = TrieEvaluator
  val scorer = new L2Scorer(edb, referenceIDB, evaluator)

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
    var (l2, numIters, gradAbs) = (tgtLoss + 1, 0, 1.0)
    val startTime = System.nanoTime()
    while (numIters < maxIters && l2 >= tgtLoss && gradAbs > 0 && step.abs > 0.0) {
      update()
      l2 = scorer.loss(currentIDB)
      numIters += 1
      gradAbs = scorer.gradientLoss(pos, currentIDB).abs
    }
    val endTime = System.nanoTime()
    val timePerIter = (endTime - startTime) / 1.0e9 / numIters
    scribe.debug(s"#Iterations: $numIters")
    scribe.debug(s"Time / iteration: $timePerIter seconds.")
    getBestIteration
  }

  def update(): Unit = {
    val oldPos = pos
    pos = newPosL2Newton
    rules = pos(rules)
    currentIDB = evaluator(rules, edb)
    step = pos - oldPos

    val l2 = scorer.loss(currentIDB)
    if (bestIteration.isEmpty || l2 < bestIteration.get._4) bestIteration = Some((pos, rules, currentIDB, l2))
  }

  def newPosL2Newton: TokenVec = {
    val l2 = scorer.loss(currentIDB)
    val grad = scorer.gradientLoss(pos, currentIDB)

    // if (grad.abs == 0) { this }
    val delta = grad.unit * l2 / grad.abs
    val newPos = pos - delta
    val newPosLim = newPos.limitLower(0.0).limitLower(0.01, pos).limitUpper(0.99, pos).limitUpper(1.0)
    val newStep = newPosLim - pos // (newPosLim - pos) * 0.8 + step * 0.2
    val bestL2 = bestIteration.map(_._4).getOrElse(Double.PositiveInfinity)
    // logger.debug(s"  grad: $grad")
    scribe.debug(s"  l2: $l2. best.l2: $bestL2. |pos|: ${newPos.abs}. |grad|: ${grad.abs}. |step|: ${newStep.abs}.")
    newPosLim
  }

  def keepHighestTokens: Seq[(TokenVec, Set[Rule[FValue]], Config[FValue], Double)] = {
    val bestPos = bestIteration.get._1
    val sortedTokens = bestPos.toSeq.sortBy(-_._2).map(_._1)

    scribe.debug("Keeping highest tokens")
    for (k <- Range(1, sortedTokens.size + 1))
    yield {
      val highestTokens = sortedTokens.take(k).toSet
      val highestRules = bestIteration.get._2.filter(_.coeff.l.toSeq.toSet.subsetOf(highestTokens))

      val highestPos = TokenVec(tokens, t => if (highestTokens.contains(t)) bestPos.map(t) else 0.0)
      val highestIDB = evaluator(highestRules, edb)
      val highestError = scorer.loss(highestIDB)

      scribe.debug(s"$k ${highestRules.size} $highestError")
      (highestPos, highestRules, highestIDB, highestError)
    }
  }

  def keepUseful: Seq[(TokenVec, Set[Rule[FValue]], Config[FValue], Double)] = {
    val kht = keepHighestTokens
    scribe.debug("Preserving useful tokens")
    for ((hipos, hirules, hiIDB, _) <- kht)
      yield {
        val usefulTokens = for (rel <- scorer.outputRels;
                                (_, v) <- hiIDB(rel).support;
                                token <- v.l.toSeq)
                           yield token

        val usefulPos = TokenVec(tokens, t => if (usefulTokens.contains(t)) hipos.map(t) else 0.0)
        val usefulRules = usefulPos(hirules).filter(_.coeff.v > 0.0)
        val usefulIDB = evaluator(usefulRules, edb)
        val usefulError = scorer.loss(usefulIDB)

        scribe.debug(s"$usefulError ${usefulRules.size}")
        (usefulPos, usefulRules, usefulIDB, usefulError)
      }
  }

  def reinterpret: Seq[(TokenVec, Set[Rule[FValue]], Config[FValue], Double)] = {
    val ku = keepUseful
    scribe.debug("Reinterpreting program")
    for ((_, urules, _, _) <- ku)
    yield {
      val utokens = urules.flatMap(_.coeff.l.toSeq)
      val rpos = TokenVec(tokens, t => if (utokens.contains(t)) 1.0 else 0.0)
      val rrules = rpos(urules)
      val rIDB = evaluator(rrules, edb)
      // val rError = scorer.loss(rIDB)
      val rf1 = scorer.f1(rIDB, 0.5)

      scribe.debug(s"$rf1 ${rrules.size}")
      (rpos, rrules, rIDB, rf1)
    }
  }

}
