package qd
package learner

class Scorer(edb: Config, refOut: Config) {
  val outputRels: Set[Relation] = refOut.keySet
  val allTuples: Map[Relation, Set[DTuple]] = outputRels.map(rel => rel -> rel.populate).toMap

  def gradient(pos: Map[Token, Double], out: Config, rel: Relation, t: DTuple): TokenVec = {
    val vt = out(rel)(t).toDouble
    val prov = out(rel)(t).prov.toSeq
    val freq = pos.map { case (token, _) => token -> prov.count(_ == token) }
    def gw(t: Token): Double = {
      val ans = freq(t) * vt / pos(t)
      if (!ans.isNaN) ans else 0.0
    }
    TokenVec(freq.map { case (token, _) => token -> gw(token) })
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // L2

  def gradL2(pos: TokenVec, out: Config): TokenVec = {
    val numeratorVecs = for (rel <- outputRels.toSeq; t <- allTuples(rel).toSeq)
                        yield gradient(pos, out, rel, t) * (out(rel)(t).toDouble - refOut(rel)(t).toDouble)
    numeratorVecs.foldLeft(TokenVec.zero(pos.keySet))(_ + _)
  }

  def errorL2(out: Config): Double = outputRels.toSeq.map(rel => errorL2(out, rel)).sum

  def errorL2(out: Config, rel: Relation): Double = {
    val allErrors = allTuples(rel).toSeq.map(t => errorL2(out, rel, t))
    allErrors.sum
  }

  def errorL2(out: Config, rel: Relation, t: DTuple): Double = {
    val vt = out(rel)(t).toDouble
    val lt = refOut(rel)(t).toDouble
    (vt - lt) * (vt - lt)
  }

  // Given a program and a cutoff value, computes the L2 loss
  // Lower is better
  def cutoffL2(p: Program, cutoff: Double): (Program, Double, Set[Token]) = {
    require(0.0 <= cutoff && cutoff <= 1.0)
    val highRules = p.rules.filter(_.coeff.toDouble >= cutoff).map(r =>
      Rule(r.name, Value(1.0, r.coeff.prov), r.head, r.body)
    )
    val pHi = Program(p.name, highRules)
    val out = SeminaiveEvaluator(pHi)(edb)
    val l2 = outputRels.toSeq.map(rel => errorL2(out, rel)).sum
    val usefulTokens = for (rel <- outputRels.toSeq; t <- allTuples(rel).toSeq; token <- out(rel)(t).prov.toSeq) yield token
    (pHi, l2, usefulTokens.toSet)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // S0 and S1

  val s0D: Double = (for (rel <- outputRels.toSeq; t <- allTuples(rel).toSeq) yield 1.0 - refOut(rel)(t).toDouble).sum
  val s1D: Double = (for (rel <- outputRels.toSeq; t <- allTuples(rel).toSeq) yield refOut(rel)(t).toDouble).sum

  def s0(out: Config): Double = {
    val s0T = for (rel <- outputRels.toSeq;
                   t <- allTuples(rel).toSeq;
                   vt = out(rel)(t).toDouble;
                   omlt = 1.0 - refOut(rel)(t).toDouble)
              yield (1.0 - vt) * omlt
    val ans = s0T.sum / s0D
    require(0.0 <= ans && ans <= 1.0)
    ans
  }

  def gradS0(pos: TokenVec, out: Config): TokenVec = {
    val numeratorVecs = for (rel <- outputRels.toSeq; t <- allTuples(rel).toSeq; omlt = 1.0 - refOut(rel)(t).toDouble)
                        yield gradient(pos, out, rel, t) * -omlt
    val numerator = numeratorVecs.foldLeft(TokenVec.zero(pos.keySet))(_ + _)
    numerator / s0D
  }

  def s1(out: Config): Double = {
    val s1T = for (rel <- outputRels.toSeq;
                   t <- allTuples(rel).toSeq;
                   vt = out(rel)(t).toDouble;
                   lt = refOut(rel)(t).toDouble)
              yield vt * lt
    val ans = s1T.sum / s1D
    require(0.0 <= ans && ans <= 1.0)
    ans
  }

  def gradS1(pos: TokenVec, out: Config): TokenVec = {
    val numeratorVecs = for (rel <- outputRels.toSeq; t <- allTuples(rel).toSeq; lt = refOut(rel)(t).toDouble)
                        yield gradient(pos, out, rel, t) * lt
    val numerator: TokenVec = numeratorVecs.foldLeft(TokenVec.zero(pos.keySet))(_ + _)
    numerator / s1D
  }
}
