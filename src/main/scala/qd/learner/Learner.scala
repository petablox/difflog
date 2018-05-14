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
  def getState: LearnerState = state
  def update(): Unit = { state = state.nextStateL2 }

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

    val score: Double = Math.min(s0, s1)

    def gradient(rel: Relation, t: DTuple): TokenVec = {
      val vt = out(rel)(t).toDouble
      val prov = out(rel)(t).prov.toSeq
      val freq = tokens.map(t => t -> prov.count(_ == t)).toMap
      TokenVec(tokens.map(t => t -> freq(t) * vt / pos(t)).toMap)
    }

    lazy val gradientS0: TokenVec = {
      val numeratorVecs = for (rel <- outputRels.toSeq;
                               (t, omlt) <- refOutCompl(rel))
                          yield gradient(rel, t) * -omlt
      val numerator = numeratorVecs.foldLeft(TokenVec.zero(tokens))(_ + _)
      numerator / s0D
    }

    lazy val gradientS1: TokenVec = {
      val numeratorVecs = for (rel <- outputRels.toSeq;
                               (t, lt) <- refOut(rel).support)
                          yield gradient(rel, t) * lt.toDouble
      val numerator: TokenVec = numeratorVecs.foldLeft(TokenVec.zero(tokens))(_ + _)
      numerator / s1D
    }

    lazy val gradientL2: TokenVec = {
      val numeratorVecs = for (rel <- outputRels.toSeq;
                               (t, lt) <- refOut(rel).support)
                          yield gradient(rel, t) * (out(rel)(t).toDouble - lt.toDouble)
      numeratorVecs.foldLeft(TokenVec.zero(tokens))(_ + _)
    }

    lazy val nextState: LearnerState = {
      val (score, grad) = if (s0 < s1) (s0, gradientS0) else (s1, gradientS1)
      // We want to make score 1
      // We have to increase by (1 - score).
      // This will need us to move by (1 - score) / |grad|
      val delta0 = grad.unit * (1 - score) / grad.abs

      def newPos(delta: TokenVec): TokenVec = pos + delta
      def newPosLim(delta: TokenVec): TokenVec = newPos(delta).limitLower(0.0).limitUpper(1.0)

      var (delta, ans) = (delta0, LearnerState(newPosLim(delta0)))
      while (ans.score < this.score && delta.abs > 0.1) {
        print(".")
        delta = delta / 2
        ans = LearnerState(newPosLim(delta))
      }

      println(s"  score: $score. s0: $s0. s1: $s1. |grad|: ${grad.abs}. numRules: ${ans.pos.count(_._2 > 0)}.")
      /* println("Token, pos, gradS0, gradS1, grad, unit, delta, newPos, newPosLim")
      for (t <- tokens.toSeq.sortBy(_.name.asInstanceOf[Int])) {
        if (pos(t) < 1.0) {
          println(s"$t, ${pos(t)}, ${gradientS0(t)}, ${gradientS1(t)}, ${grad(t)}, ${grad.unit(t)}, ${delta(t)}, ${newPos(t)}, ${newPosLim(t)}")
        }
      } */

      ans
    }

    lazy val nextStateL2: LearnerState = {
      val (score, grad) = (errorL2Total, gradientL2)
      if (grad.abs == 0) { this }
      val delta0 = grad.unit / grad.abs * errorL2Total

      def newPos(delta: TokenVec) : TokenVec = pos + delta
      def newPosLim(delta: TokenVec): TokenVec = newPos(delta).limitLower(0.0).limitUpper(1.0)

      var (delta, ans) = (delta0, LearnerState(newPosLim(delta0)))
      while (ans.score < this.score && delta.abs > 0.1) {
        print(".")
        delta = delta / 2
        ans = LearnerState(newPosLim(delta))
      }

      println(s" grad: ${grad}")
      println(s"  score: $score. s0: $s0. s1: $s1. |grad|: ${grad.abs}. numRules: ${ans.pos.count(_._2 > 0)}.")
      /* println("Token, pos, gradS0, gradS1, grad, unit, delta, newPos, newPosLim")
      for (t <- tokens.toSeq.sortBy(_.name.asInstanceOf[Int])) {
        if (pos(t) < 1.0) {
          println(s"$t, ${pos(t)}, ${gradientS0(t)}, ${gradientS1(t)}, ${grad(t)}, ${grad.unit(t)}, ${delta(t)}, ${newPos(t)}, ${newPosLim(t)}")
        }
      } */

      ans
    }

    lazy val settle: LearnerState = {
      val usefulTokens = for (rel <- outputRels; t <- out(rel).support; token <- t._2.prov.toSeq) yield token
      println(usefulTokens)
      val newMap = for ((key, value) <- pos.map) yield key -> (if (usefulTokens.contains(key)) value else 0.0)
      LearnerState(TokenVec(newMap))
    }

    lazy val extreme: LearnerState = {
      val newMap = for ((key, value) <- pos.map) yield key -> (if (value > 0.0) 1.0 else 0.0)
      LearnerState(TokenVec(newMap))
    }

    def errorL2(rel: Relation, t: DTuple): Double = {
      val vt = out(rel)(t).toDouble
      val ct = refOut(rel)(t).toDouble
      (vt - ct) * (vt - ct)
    }

    val errorL2Total = {
      val errors = for (rel <- outputRels.toSeq; t <- out(rel).support) yield errorL2(rel, t._1)
      errors.sum
    }
  }
}
