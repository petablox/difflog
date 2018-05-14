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
      val ans = s0T.sum / s0D
      require(0.0 <= ans && ans <= 1.0)
      ans
    }

    val s1: Double = {
      val s1T = for (relation <- outputRels.toSeq;
                     (t, lt) <- refOut(relation).support;
                     vt = out(relation)(t).toDouble)
                yield vt * lt.toDouble
      val ans = s1T.sum / s1D
      require(0.0 <= ans && ans <= 1.0)
      ans
    }

    def gradient(rel: Relation, t: DTuple): TokenVec = {
      val vt = out(rel)(t).toDouble
      val prov = out(rel)(t).prov.toSeq
      val freq = tokens.map(t => t -> prov.count(_ == t)).toMap
      TokenVec(tokens.map(t => t -> freq(t) * vt / pos(t)).toMap)
    }

    def gradientS0: TokenVec = {
      val numeratorVecs = for (rel <- outputRels.toSeq;
                               (t, omlt) <- refOutCompl(rel))
                          yield gradient(rel, t) * omlt
      val numerator = numeratorVecs.foldLeft(TokenVec.zero(tokens))(_ + _)
      numerator / s0D
    }

    def gradientS1: TokenVec = {
      val numeratorVecs = for (rel <- outputRels.toSeq;
                               (t, lt) <- refOut(rel).support)
                          yield gradient(rel, t) * lt.toDouble
      val numerator: TokenVec = numeratorVecs.foldLeft(TokenVec.zero(tokens))(_ + _)
      numerator / s1D
    }

    def nextState: LearnerState = {
      val (score, grad) = if (s0 < s1) (s0, gradientS0) else (s1, gradientS1)
      // We want to make score 1
      // We have to increase by (1 - score).
      // This will need us to move by (1 - score) / |grad|
      val delta = grad.unit * (1 - score) / grad.abs
      LearnerState(pos + delta)
    }

    def errorL2(rel: Relation, t: DTuple) : Double = {
      val vt = out(rel)(t).toDouble
      val ct = refOut(rel)(t).toDouble
      (vt - ct) * (vt - ct)
    }
  }

  println(s"s0: ${state.s0}")
  println(s"s1: ${state.s1}")
}
