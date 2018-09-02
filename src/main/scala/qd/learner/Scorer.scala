package qd
package learner

import qd.Semiring.FValueSemiringObj
import qd.evaluator.Evaluator

class Scorer(edb: Config[FValue], refIDB: Config[FValue], evaluator: Evaluator) {

  implicit val vs: FValueSemiring = FValueSemiringObj
  val outputRels: Set[Relation] = refIDB.map.keySet

  def gradient(pos: TokenVec, out: Config[FValue], rel: Relation, t: DTuple): TokenVec = {
    val vt = out(rel)(t).v
    val prov = out(rel)(t).l.toSeq
    val freq = pos.keySet.map(token => token -> prov.count(_ == token)).toMap
    def gw(t: Token): Double = {
      val ans = freq(t) * vt / pos(t).v
      if (!ans.isNaN) ans else 0.0
    }
    TokenVec(freq.map { case (token, _) => token -> gw(token) })
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // L2

  def gradL2(pos: TokenVec, out: Config[FValue]): TokenVec = {
    val numeratorVecs = for (rel <- outputRels.toSeq;
                             allTuples = out(rel).support.map(_._1) ++ refIDB(rel).support.map(_._1);
                             t <- allTuples)
                        yield gradient(pos, out, rel, t) * (out(rel)(t).v - refIDB(rel)(t).v)
    numeratorVecs.foldLeft(TokenVec.zero(pos.keySet))(_ + _)
  }

  def errorL2(out: Config[FValue]): Double = outputRels.toSeq.map(rel => errorL2(out, rel)).sum

  def errorL2(out: Config[FValue], rel: Relation): Double = {
    val allTuples = out(rel).support.map(_._1) ++ refIDB(rel).support.map(_._1)
    val allErrors = allTuples.toSeq.map(t => errorL2(out, rel, t))
    allErrors.sum
  }

  def errorL2(out: Config[FValue], rel: Relation, t: DTuple): Double = {
    val vt = out(rel)(t).v
    val lt = refIDB(rel)(t).v
    (vt - lt) * (vt - lt)
  }

  // Given a program and a cutoff value, computes the L2 loss
  // Lower is better
  def cutoffL2(p: Program[FValue], cutoff: Double): (Program[FValue], Double, Set[Token]) = {
    require(0.0 <= cutoff && cutoff <= 1.0)
    val highRules = p.rules.filter(_.coeff.v >= cutoff).map(r =>
      Rule(FValue(1.0, r.coeff.l), r.head, r.body)
    )
    val pHi = Program(p.name, highRules)
    val out = evaluator(pHi, edb)
    val l2 = outputRels.toSeq.map(rel => errorL2(out, rel)).sum
    val usefulTokens = for (rel <- outputRels.toSeq;
                            allTuples = out(rel).support.map(_._1) ++ refIDB(rel).support.map(_._1);
                            t <- allTuples.toSeq;
                            token <- out(rel)(t).l.toSeq)
                       yield token
    (pHi, l2, usefulTokens.toSet)
  }

}
