package qd
package learner

import scala.util.Random

class Learner(edb: Config, refOut: Config, p0: Program, random: Random) {
  val scorer = new Scorer(edb, refOut)
  val tokens: Set[Token] = p0.rules.flatMap(_.coeff.prov.toSeq.toSet)
  require(tokens.nonEmpty)

  private var pos: TokenVec = TokenVec(tokens, random)
  private var p: Program = pos.reorient(p0)
  private var evaluator: Evaluator = SeminaiveEvaluator(p)
  private var out: Config = evaluator(edb)
  private var best: Option[(Program, TokenVec, Config, Double)] = None
  private var step: TokenVec = TokenVec(tokens.map(token => token -> 1.0).toMap)

  def getPos: TokenVec = pos
  def getProgram: Program = p
  def getOutput: Config = out
  def getBest: (Program, TokenVec, Config, Double) = best.get

  def learn(tgtLoss: Double, maxIters: Int): (Program, TokenVec, Config, Double) = {
    require(maxIters > 0)
    var (l2, numIters, gradAbs) = (tgtLoss + 1, 0, 1.0)
    while (numIters < maxIters && l2 >= tgtLoss && gradAbs > 0) {
      update()
      l2 = scorer.errorL2(out)
      numIters += 1
      gradAbs = scorer.gradL2(pos, out).abs
    }
    getBest
  }

  def update(): Unit = {
    val oldPos = pos
    pos = newPosL2Newton
    p = pos.reorient(p)
    evaluator = SeminaiveEvaluator(p)
    out = evaluator(edb)
    step = pos - oldPos

    val l2 = scorer.errorL2(out)
    if (best.isEmpty || l2 < best.get._4) best = Some((p, pos, out, l2))
  }

  def newPosS0S1Newton(): TokenVec = {
    val (s0, gradS0) = (scorer.s0(out), scorer.gradS0(pos, out))
    val (s1, gradS1) = (scorer.s1(out), scorer.gradS1(pos, out))
    val (score, grad) = if (s0 < s1) (s0, gradS0) else (s1, gradS1)

    // We want to make score 1
    // We have to increase by (1 - score).
    // This will need us to move by (1 - score) / |grad|
    val delta = grad.unit * (1 - score) / grad.abs
    val newPos = pos + delta
    val newPosLim = newPos.limitLower(0.0).limitLower(0.01, pos).limitUpper(0.99, pos).limitUpper(1.0)
    val step = newPosLim - pos
    val longestRule = evaluator.getTime.toSeq.minBy(-_._2)
    println(s"  score: $score. s0: $s0. s1: $s1. |grad|: ${grad.abs}. |step|: ${step.abs}. " +
            s"Longest rule: ${longestRule._1.name} needed ${longestRule._2 / 1.0e9} seconds.")

    newPosLim
  }

  def newPosL2Newton: TokenVec = {
    val l2 = scorer.errorL2(out)
    val grad = scorer.gradL2(pos, out)

    // if (grad.abs == 0) { this }
    val delta = grad.unit * l2 / grad.abs
    val newPos = pos - delta
    val newPosLim = newPos.limitLower(0.0).limitLower(0.01, pos).limitUpper(0.99, pos).limitUpper(1.0)
    val step = newPosLim - pos
    val bestL2 = best.map(_._4).getOrElse(Double.PositiveInfinity)
    val longestRule = evaluator.getTime.toSeq.minBy(-_._2)
    // println(s"  grad: $grad")
    println(s"  l2: $l2. best.l2: $bestL2. |grad|: ${grad.abs}. |step|: ${step.abs}. " +
            s"Longest rule: ${longestRule._1.name} needed ${longestRule._2 / 1.0e9} seconds.")
    newPosLim
  }

  def reinterpretL2(cutoff: Double): (Program, Double, Set[Token]) = scorer.cutoffL2(p, cutoff)

  def printWeights(): Unit = {
    for (r <- p.rules) println(s"name: ${r.name}, coeff: ${r.coeff}")
  }
}
