package qd
package learner

import scala.util.Random

class Learner(edb: Config, refOut: Config, p0: Program, random: Random) {
  val scorer = new Scorer(edb, refOut)
  val tokens: Set[Token] = p0.rules.flatMap(_.coeff.prov.toSeq.toSet)
  require(tokens.nonEmpty)

  private var pos: TokenVec = TokenVec(tokens, random)
  private var p: Program = pos.reorient(p0)
  private var out: Config = SeminaiveEvaluator(p)(edb)

  def update(): Unit = {
    pos = newPosL2Newton
    p = pos.reorient(p)
    out = SeminaiveEvaluator(p)(edb)
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
    val newPosLim = newPos.limitLower(0.0).limitUpper(1.0)
    val step = newPosLim - pos
    println(s"  score: $score. s0: $s0. s1: $s1. |grad|: ${grad.abs}. |step|: ${step.abs}")

    newPosLim
  }

  def newPosL2Newton: TokenVec = {
    val grad = scorer.gradL2(pos, out)
    // if (grad.abs == 0) { this }
    val delta = grad.unit * scorer.errorL2(out) / grad.abs
    val newPos = pos - delta
    val newPosLim = newPos.limitLower(0.0).limitUpper(1.0)
    val step = newPosLim - pos
    // println(s"  grad: $grad")
    println(s"  |grad|: ${grad.abs}. |step|: ${step.abs}")
    newPosLim
  }

  def reinterpretL2(cutoff: Double): (Program, Double) = scorer.cutoffL2(p, cutoff)
}
