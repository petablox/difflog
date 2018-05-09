package qd
package learner

import scala.util.Random

class Learner(edb: Config, refOut: Config, p0: Program, random: Random) {
  val outputRels: Set[Relation] = refOut.keySet
  val tokens: Set[Token] = p0.rules.flatMap(_.coeff.prov.toSeq.toSet)
  require(tokens.nonEmpty)

  val refOutCompl: Map[Relation, Map[DTuple, Double]] = {
    val ans = for (rel <- outputRels)
              yield rel -> rel.populate.map(t => t -> (1.0 - refOut(rel)(t).toDouble)).toMap
    ans.toMap
  }
  val s0D: Double = (for (relation <- outputRels.toSeq;
                          (_, omlt) <- refOutCompl(relation))
                     yield omlt).sum
  val s1D: Double = (for (relation <- outputRels.toSeq;
                          (_, lt) <- refOut(relation).support)
                     yield lt.toDouble).sum

  private var state = LearnerState(TokenVec(tokens, random))
  case class LearnerState(pos: TokenVec) {
    val p: Program = pos.reorient(p0)
    val out: Config = SeminaiveEvaluator(p)(edb)

    val s0: Double = {
      val s0T = for (relation <- outputRels.toSeq;
                     (t, omlt) <- refOutCompl(relation);
                     vt = out(relation)(t).toDouble)
                yield (1.0 - vt) * omlt
      s0T.sum / s0D
    }

    val s1: Double = {
      val s1T = for (relation <- outputRels.toSeq;
                     (t, lt) <- refOut(relation).support;
                     vt = out(relation)(t).toDouble)
                yield vt * lt.toDouble
      s1T.sum / s1D
    }

    def gradient(rel: Relation, t: DTuple): TokenVec = {
      val vt = out(rel)(t).toDouble
      val prov = out(rel)(t).prov.toSeq
      val freq = tokens.map(t => t -> prov.count(_ == t)).toMap
      TokenVec(tokens.map(t => t -> freq(t) * vt / pos(t)).toMap)
    }
  }

  println(s"s0: ${state.s0}")
  println(s"s1: ${state.s1}")
}
