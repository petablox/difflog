package qd.learner

import qd.Semiring.FValueSemiringObj
import qd._
import qd.evaluator.Evaluator
import qd.instance.Config

abstract class Scorer {

  val edb: Config[FValue]
  val refIDB: Config[FValue]
  val evaluator: Evaluator

  implicit val vs: FValueSemiring = FValueSemiringObj
  val outputRels: Set[Relation] = refIDB.map.keySet

  def gradient(pos: TokenVec, out: Config[FValue], rel: Relation, t: DTuple): TokenVec = {
    val vt = out(rel)(t).v
    val prov = out(rel)(t).l.toVector
    val freq = pos.keySet.map(token => token -> prov.count(_ == token)).toMap
    def gw(t: Token): Double = {
      val ans = freq(t) * vt / pos(t).v
      if (!ans.isNaN) ans else 0.0
    }
    TokenVec(freq.map { case (token, _) => token -> gw(token) })
  }

  def loss(out: Config[FValue]): Double = outputRels.toSeq.map(rel => loss(out, rel)).sum

  def loss(out: Config[FValue], rel: Relation): Double = {
    val allTuples = out(rel).support.map(_._1) ++ refIDB(rel).support.map(_._1)
    val allErrors = allTuples.toSeq.map(t => loss(out, rel, t))
    allErrors.sum
  }

  def loss(out: Config[FValue], rel: Relation, t: DTuple): Double

  def gradientLoss(pos: TokenVec, out: Config[FValue]): TokenVec = {
    val numeratorVecs = for (rel <- outputRels.toSeq;
                             allTuples = out(rel).support.map(_._1) ++ refIDB(rel).support.map(_._1);
                             t <- allTuples.toSeq)
                        yield gradientLoss(pos, out, rel, t)
    numeratorVecs.foldLeft(TokenVec.zero(pos.keySet))(_ + _)
  }

  def gradientLoss(pos: TokenVec, out: Config[FValue], rel: Relation, t: DTuple): TokenVec

  def f1(out: Config[FValue], cutoff: Double): Double = {
    val p = precision(out, cutoff)
    val r = recall(out, cutoff)
    2 * p * r / (p + r)
  }

  def precision(out: Config[FValue], cutoff: Double): Double = {
    val ts = for (rel <- outputRels.toSeq;
                  (t, v) <- out(rel).support.toSeq;
                  if v.v > cutoff)
             yield if (refIDB(rel)(t).v > cutoff) 1.0 else 0.0
    ts.sum / ts.size
  }

  def recall(out: Config[FValue], cutoff: Double): Double = {
    val ts = for (rel <- outputRels.toSeq;
                  (t, v) <- refIDB(rel).support.toSeq;
                  if v.v > cutoff)
      yield if (out(rel)(t).v > cutoff) 1.0 else 0.0
    ts.sum / ts.size
  }

}
