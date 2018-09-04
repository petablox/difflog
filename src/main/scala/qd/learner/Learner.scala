package qd
package learner

import com.typesafe.scalalogging.Logger
import qd.Semiring.FValueSemiringObj
import qd.evaluator.{Evaluator, TrieEvaluator}

class Learner(q0: Problem) {

  implicit val vs: FValueSemiring = FValueSemiringObj
  private val logger = Logger[Learner]

  val edb: Config[FValue] = q0.edbConfig
  val referenceIDB: Config[FValue] = q0.idbConfig

  val tokens: Set[Token] = q0.allTokens
  val evaluator: Evaluator = TrieEvaluator
  val scorer = new L2Scorer(edb, referenceIDB, evaluator)

  private var pos: TokenVec = q0.pos
  private var program: Program[FValue] = q0.program
  private var currentIDB: Config[FValue] = evaluator(program, edb)
  private var bestIteration: Option[(TokenVec, Program[FValue], Config[FValue], Double)] = None
  private var step: TokenVec = TokenVec(tokens.map(token => token -> 1.0).toMap)

  def getPos: TokenVec = pos
  def getProgram: Program[FValue] = program
  def getCurrentIDB: Config[FValue] = currentIDB
  def getBestIteration: (TokenVec, Program[FValue], Config[FValue], Double) = bestIteration.get

  def learn(tgtLoss: Double, maxIters: Int): (TokenVec, Program[FValue], Config[FValue], Double) = {
    require(maxIters > 0)
    var (l2, numIters, gradAbs) = (tgtLoss + 1, 0, 1.0)
    while (numIters < maxIters && l2 >= tgtLoss && gradAbs > 0 && step.abs > 0.0) {
      update()
      l2 = scorer.loss(currentIDB)
      numIters += 1
      gradAbs = scorer.gradientLoss(pos, currentIDB).abs
    }
    logger.debug(s"#Iterations: $numIters")
    getBestIteration
  }

  def update(): Unit = {
    val oldPos = pos
    pos = newPosL2Newton
    program = pos(program)
    currentIDB = evaluator(program, edb)
    step = pos - oldPos

    val l2 = scorer.loss(currentIDB)
    if (bestIteration.isEmpty || l2 < bestIteration.get._4) bestIteration = Some((pos, program, currentIDB, l2))
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
    logger.debug(s"  l2: $l2. best.l2: $bestL2. |grad|: ${grad.abs}. |step|: ${newStep.abs}.")
    newPosLim
  }

  def keepHighestTokens: Seq[(TokenVec, Program[FValue], Config[FValue], Double)] = {
    val bestPos = bestIteration.get._1
    val sortedTokens = bestPos.toSeq.sortBy(-_._2).map(_._1)

    logger.debug("Keeping highest tokens")
    for (k <- Range(1, sortedTokens.size + 1))
    yield {
      val highestTokens = sortedTokens.take(k).toSet
      val highestRules = bestIteration.get._2.rules.filter(_.coeff.l.toSeq.toSet.subsetOf(highestTokens))

      val highestPos = TokenVec(tokens, t => if (highestTokens.contains(t)) bestPos.map(t) else 0.0)
      val highestProgram = Program(s"PH$k", highestRules)
      val highestIDB = evaluator(highestRules, edb)
      val highestError = scorer.loss(highestIDB)

      logger.debug(s"${highestProgram.name} ${highestRules.size} $highestError")
      (highestPos, highestProgram, highestIDB, highestError)
    }
  }

  def keepUseful: Seq[(TokenVec, Program[FValue], Config[FValue], Double)] = {
    val kht = keepHighestTokens
    logger.debug("Preserving useful tokens")
    for ((hipos, hiprog, hiIDB, _) <- kht)
      yield {
        val usefulTokens = for (rel <- scorer.outputRels;
                                (_, v) <- hiIDB(rel).support;
                                token <- v.l.toSeq)
                           yield token

        val usefulPos = TokenVec(tokens, t => if (usefulTokens.contains(t)) hipos.map(t) else 0.0)
        val usefulRules = usefulPos(hiprog).rules.filter(_.coeff.v > 0.0)
        val usefulProgram = Program(s"U${hiprog.name}", usefulRules)
        val usefulIDB = evaluator(usefulRules, edb)
        val usefulError = scorer.loss(usefulIDB)

        logger.debug(s"${usefulProgram.name} $usefulError ${usefulProgram.rules.size}")
        (usefulPos, usefulProgram, usefulIDB, usefulError)
      }
  }

  def reinterpret: Seq[(TokenVec, Program[FValue], Config[FValue], Double)] = {
    val ku = keepUseful
    logger.debug("Reinterpreting program")
    for ((_, uprog, _, _) <- ku)
    yield {
      val utokens = uprog.rules.flatMap(_.coeff.l.toSeq)
      val rpos = TokenVec(tokens, t => if (utokens.contains(t)) 1.0 else 0.0)
      val rprog = rpos(uprog)
      val rIDB = evaluator(rprog, edb)
      val rError = scorer.loss(rIDB)

      logger.debug(s"${rprog.name} $rError ${rprog.rules.size}")
      (rpos, rprog, rIDB, rError)
    }
  }

}
