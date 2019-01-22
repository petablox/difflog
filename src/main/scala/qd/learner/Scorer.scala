package qd
package learner

import qd.instance.Config
import qd.tokenvec.TokenVec

abstract class Scorer {

  def gradient(pos: TokenVec, cOut: Config[FValue], rel: Relation, t: DTuple): TokenVec = {
    val vt = cOut(rel)(t).v
    val prov = cOut(rel)(t).l.toVector
    val freq = prov.groupBy(identity).map { case (token, value) => token -> value.size }
    val ans = pos.keySet.map { token =>
      val dvtDtoken = freq.getOrElse(token, 0) * vt / pos(token).v
      token -> (if (!dvtDtoken.isNaN) dvtDtoken else 0.0)
    }
    TokenVec(ans.toMap)
  }

  def loss(vOut: Double, vRef: Double): Double

  def loss(cOut: Config[FValue], cRef: Config[FValue], rel: Relation, t: DTuple): Double = {
    val vOut = cOut(rel)(t).v
    val vRef = cRef(rel)(t).v
    loss(vOut, vRef)
  }

  def loss(cOut: Config[FValue], cRef: Config[FValue], rel: Relation): Double = {
    val allTuples = cOut(rel).support.map(_._1) ++ cRef(rel).support.map(_._1)
    val allErrors = allTuples.map(t => loss(cOut, cRef, rel, t))
    allErrors.sum
  }

  def loss(cOut: Config[FValue], cRef: Config[FValue], outputRels: Set[Relation]): Double = {
    outputRels.map(rel => loss(cOut, cRef, rel)).sum
  }

  def gradientLoss(pos: TokenVec, cOut: Config[FValue], cRef: Config[FValue], rel: Relation, t: DTuple): TokenVec

  def gradientLoss(pos: TokenVec, cOut: Config[FValue], cRef: Config[FValue], outputRels: Set[Relation]): TokenVec = {
    val numeratorVecs = for (rel <- outputRels.toSeq;
                             allTuples = cOut(rel).support.map(_._1) ++ cRef(rel).support.map(_._1);
                             t <- allTuples)
                        yield gradientLoss(pos, cOut, cRef, rel, t)
    numeratorVecs.foldLeft(TokenVec.zero(pos.keySet))(_ + _)
  }

  def f1(cOut: Config[FValue], cRef: Config[FValue], outputRels: Set[Relation], cutoff: Double): Double = {
    val p = precision(cOut, cRef, outputRels, cutoff)
    val r = recall(cOut, cRef, outputRels, cutoff)
    2 * p * r / (p + r)
  }

  def precision(cOut: Config[FValue], cRef: Config[FValue], outputRels: Set[Relation], cutoff: Double): Double = {
    val ts = for (rel <- outputRels.toSeq;
                  (t, v) <- cOut(rel).support
                  if v.v > cutoff)
             yield if (cRef(rel)(t).v > cutoff) 1.0 else 0.0
    ts.sum / ts.size
  }

  def recall(cOut: Config[FValue], cRef: Config[FValue], outputRels: Set[Relation], cutoff: Double): Double = {
    val ts = for (rel <- outputRels.toSeq;
                  (t, v) <- cRef(rel).support
                  if v.v > cutoff)
             yield if (cOut(rel)(t).v > cutoff) 1.0 else 0.0
    ts.sum / ts.size
  }

}

object Scorer {
  val STD_SCORERS: Map[String, Scorer] = Map("l2" -> L2Scorer, "xentropy" -> XEntropyScorer)
}

object L2Scorer extends Scorer {

  override def loss(vOut: Double, vRef: Double): Double = (vOut - vRef) * (vOut - vRef)

  override def gradientLoss(
                             pos: TokenVec,
                             cOut: Config[FValue],
                             cRef: Config[FValue],
                             rel: Relation,
                             t: DTuple
                           ): TokenVec = {
    gradient(pos, cOut, rel, t) * (cOut(rel)(t).v - cRef(rel)(t).v) * 2
  }

}

object XEntropyScorer extends Scorer {

  override def loss(vOut: Double, vRef: Double): Double = -(vRef * Math.log(vOut) + (1 - vRef) * Math.log(1 - vOut))

  override def gradientLoss(
                             pos: TokenVec,
                             cOut: Config[FValue],
                             cRef: Config[FValue],
                             rel: Relation,
                             t: DTuple
                           ): TokenVec = {
    val vt = cOut(rel)(t).v
    val lt = cRef(rel)(t).v
    val gradv = gradient(pos, cOut, rel, t)
    gradv * (vt - lt) / vt / (1 - vt)
  }

}
