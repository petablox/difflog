package qd
package learner

import qd.evaluator.Evaluator
import qd.instance.Config

class L2Scorer(val edb: Config[FValue], val refIDB: Config[FValue], val evaluator: Evaluator) extends Scorer {

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // L2

  override def loss(out: Config[FValue], rel: Relation, t: DTuple): Double = {
    val vt = out(rel)(t).v
    val lt = refIDB(rel)(t).v
    (vt - lt) * (vt - lt)
  }

  def gradientLoss(pos: TokenVec, out: Config[FValue], rel: Relation, t: DTuple): TokenVec = {
    gradient(pos, out, rel, t) * (out(rel)(t).v - refIDB(rel)(t).v) * 2
  }

  // Given a program and a cutoff value, computes the L2 loss
  // Lower is better
  def cutoffL2(rules: Set[Rule[FValue]], cutoff: Double): (Set[Rule[FValue]], Double, Set[Token]) = {
    require(0.0 <= cutoff && cutoff <= 1.0)
    val highRules = rules.filter(_.coeff.v >= cutoff).map(r =>
      Rule(FValue(1.0, r.coeff.l), r.head, r.body)
    )
    val out = evaluator(highRules, edb)
    val l2 = outputRels.toSeq.map(rel => loss(out, rel)).sum
    val usefulTokens = for (rel <- outputRels.toSeq;
                            allTuples = out(rel).support.map(_._1) ++ refIDB(rel).support.map(_._1);
                            t <- allTuples.toSeq;
                            token <- out(rel)(t).l.toVector)
                       yield token
    (highRules, l2, usefulTokens.toSet)
  }

}
