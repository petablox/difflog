package qd
package learner

import org.scalatest.FunSuite

import scala.util.{Random, Try}

abstract class Problem extends FunSuite {
  def name: Any
  def edb: Config
  def refOut: Config
  def soup: Set[Rule]
  def expected: Set[Any]
  def maxVarCount: Int

  def p0: Program = Program(s"Soup-$name", soup.filter(_.freeVariables.size <= maxVarCount))
  lazy val scorer = new Scorer(edb, refOut)

  test("Counting number of free variables in each rule") {
    var idx = 0
    for (rule <- soup.toSeq.sortBy(-_.freeVariables.size)) {
      val desired = if (expected.contains(rule.name)) "EXP" else "UXP"
      println(s"Rulevars $idx ${rule.name} $desired ${rule.freeVariables.size}")
      idx += 1
    }
  }

  test(s"Applying seminaive evaluator to initial program of $name") {
    val startTime = System.nanoTime()
    val evaluator = SeminaiveEvaluator(p0)
    val out = evaluator(edb)
    val endTime = System.nanoTime()
    val rmse = scorer.rmse(out)
    println(s"Evaluation finished in ${(endTime - startTime) / 1.0e9} seconds.")
    println(s"RMS Error : ${rmse}.")
  }

  test(s"Estimating goodness of initial program of $name") {
    for (cutoff <- Set(0.2, 0.3, 0.6)) {
      val (_, l2, _) = scorer.cutoffL2(p0, cutoff)
      println(s"cutoff: $cutoff. l2: $l2")
    }
  }

  test(s"Estimating goodness of expected program for $name") {
    val startTime = System.nanoTime()
    val p = Program(p0.name,
                    p0.rules.filter(r => expected.contains(r.name))
                            .map(r => Rule(r.name, Value(1.0, r.coeff.prov), r.head, r.body)))
    val evaluator = SeminaiveEvaluator(p)
    val out = evaluator(edb)
    val l2 = scorer.errorL2(out)
    val endTime = System.nanoTime()
    println(s"L2: $l2. Evaluation finished in ${(endTime - startTime) / 1.0e9} seconds.")
  }

  test(s"Learning $name") {
    val seed = Try(sys.env.getOrElse("RANDOM_SEED", "0").toInt).getOrElse(0)
    val random: Random = new Random(seed)
    val learner = new Learner(edb, refOut, p0, random)
    learner.learn(0.01, 500)
    println(s"Best program: ${learner.getBest._2.toSeq.sortBy(_._1.name.asInstanceOf[Int])}")
    println(s"Final program: ${learner.getPos.toSeq.sortBy(_._1.name.asInstanceOf[Int])}")
    println(s"Expected: ${expected.toSeq.sortBy(_.asInstanceOf[Int])}")

    println("Final program")
    for (cutoff <- Range(0, 11).map(_ / 10.0)) {
      val (p, l2, usefulTokens) = learner.reinterpretL2(cutoff)
      val coeffs = p.rules.toSeq.sortBy(_.name.asInstanceOf[Int]).map(r => r.name)
      println(s"cutoff: $cutoff. l2: $l2. usefulTokens: ${usefulTokens.toSeq.sortBy(_.name.asInstanceOf[Int]).mkString(", ")}")
    }

    println("Best program")
    val bestP = learner.getBest
    for (cutoff <- Range(0, 11).map(_ / 10.0)) {
      val (p, l2, usefulTokens) = scorer.cutoffL2(bestP._1, cutoff)
      val coeffs = p.rules.toSeq.sortBy(_.name.asInstanceOf[Int]).map(r => r.name)
      println(s"cutoff: $cutoff. l2: $l2. usefulTokens: ${usefulTokens.toSeq.sortBy(_.name.asInstanceOf[Int]).mkString(", ")}")
    }
  }
}
