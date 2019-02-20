package qd
package learner

import qd.Semiring.FValueSemiringObj
import qd.dgraph.Derivation.StochasticConfig
import qd.instance.Config
import qd.tokenvec.TokenVec
import qd.util.Contract

abstract class StochasticScorer {

  override def toString: String

  def gradient(pos: TokenVec, cOut: StochasticConfig, relation: Relation, tuple: DTuple): TokenVec = {
    val pathSamples = cOut(relation)(tuple)

    val gradients = pathSamples.map(path => {
      val pathValues = path.map(clause => Value(clause.rule.lineage, pos))
      val pathValue = pathValues.foldLeft(FValueSemiringObj.One)(_ * _)

      val vt = pathValue.v
      val prov = pathValue.l.toVector
      val freq = prov.groupBy(identity).map { case (token, value) => token -> value.size }
      val ans = pos.keySet.map { token =>
        val dvtDtoken = freq.getOrElse(token, 0) * vt / pos(token).v
        token -> (if (!dvtDtoken.isNaN) dvtDtoken else 0.0)
      }
      TokenVec(ans.toMap)
    })
    val totalGradient = gradients.foldLeft(TokenVec(pos.keySet, _ => 0))(_ + _)
    val ans = totalGradient * 1.0 / pathSamples.size
    ans
  }

  def loss(vOut: Double, vRef: Double): Double

  def loss(cOut: StochasticConfig, cRef: Config[FValue], rel: Relation, t: DTuple): Double = {
    val vOut = cOut(rel)(t).v
    val vRef = cRef(rel)(t).v
    loss(vOut, vRef)
  }

  def loss(cOut: StochasticConfig, cRef: Config[FValue], rel: Relation): Double = {
    val allTuples = cOut(rel).support.map(_._1) ++ cRef(rel).support.map(_._1)
    val allErrors = allTuples.map(t => loss(cOut, cRef, rel, t))
    allErrors.sum
  }

  def loss(cOut: StochasticConfig, cRef: Config[FValue], outputRels: Set[Relation]): Double = {
    outputRels.map(rel => loss(cOut, cRef, rel)).sum
  }

  def gradientLoss(pos: TokenVec, cOut: StochasticConfig, cRef: Config[FValue], rel: Relation, t: DTuple): TokenVec

  def gradientLoss(pos: TokenVec, cOut: StochasticConfig, cRef: Config[FValue], outputRels: Set[Relation]): TokenVec = {
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

object StochasticScorer {
  val STD_SCORERS: Map[String, Scorer] = Set(L2Scorer, XEntropyScorer).map(scorer => scorer.toString -> scorer).toMap
}

object StochasticL2Scorer extends StochasticScorer {

  override def toString: String = "StochasticL2Scorer"

  override def loss(vOut: Double, vRef: Double): Double = (vOut - vRef) * (vOut - vRef)

  override def gradientLoss(
                             pos: TokenVec,
                             cOut: StochasticConfig,
                             cRef: Config[FValue],
                             rel: Relation,
                             tuple: DTuple
                           ): TokenVec = {
    val pathSamples = cOut(rel)(tuple)
    val pathValues = pathSamples.map(path => {
      path.map(clause => Value(clause.rule.lineage, pos)).foldLeft(FValueSemiringObj.One)(_ * _).v
    })
    val avgPathValue = pathValues.sum / pathValues.size
    gradient(pos, cOut, rel, tuple) * (avgPathValue - cRef(rel)(tuple).v) * 2
  }

}

object XEntropyScorer extends StochasticScorer {

  override def toString: String = "XEntropyScorer"

  override def loss(vOut: Double, vRef: Double): Double = {
    if (vRef == 0) -Math.log(1 - vOut)
    else if (vRef == 1) -Math.log(vOut)
    else -(vRef * Math.log(vOut) + (1 - vRef) * Math.log(1 - vOut))
  }

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
    val ans = gradv * (vt - lt) / vt / (1 - vt)
    Contract.assert(ans.forall(v => java.lang.Double.isFinite(v._2)))
    ans
  }

}
