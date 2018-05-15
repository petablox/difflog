package qd
package learner

class Scorer(edb: Config, refOut: Config) {
  val outputRels: Set[Relation] = refOut.keySet
  val refOutCompl: Map[Relation, Map[DTuple, Double]] = {
    val ans = for (rel <- outputRels)
              yield rel -> rel.populate.map(t => t -> (1.0 - refOut(rel)(t).toDouble)).toMap
    ans.toMap
  }

  def errorL2(out: Config, rel: Relation, t: DTuple): Double = {
    val vt = out(rel)(t).toDouble
    val lt = refOut(rel)(t).toDouble
    (vt - lt) * (vt - lt)
  }

  def errorL2(out: Config, rel: Relation): Double = {
    val errors = for (rel <- outputRels.toSeq; (t, _) <- refOutCompl(rel)) yield errorL2(out, rel, t)
    errors.sum
  }

  // Given a program and a cutoff value, computes the L2 loss
  // Lower is better
  def cutoffL2(p: Program, cutoff: Double): Double = {
    require(0.0 <= cutoff && cutoff <= 1.0)
    val highRules = p.rules.filter(_.coeff.toDouble >= cutoff).map(r =>
      Rule(r.name, Value(1.0, r.coeff.prov), r.head, r.body)
    )
    val highP = Program(p.name, highRules)
    println(highP.rules.map(_.name))
    val out: Config = SeminaiveEvaluator(highP)(edb)
    outputRels.toSeq.map(rel => errorL2(out, rel)).sum
  }
}
