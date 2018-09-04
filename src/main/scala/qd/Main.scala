package qd

import qd.Semiring.FValueSemiringObj
import qd.evaluator.TrieSemiEvaluator
import qd.learner.Learner

import scala.io.Source

object Main extends App {

  implicit val vs: FValueSemiring = FValueSemiringObj

  val parser = new Parser()
  val input = scala.io.Source.fromInputStream(System.in).mkString
  val problem = parser.parseAll(parser.problem, input).get

  def eval(): Unit = {
    val edbMap: Map[Relation, Instance[FValue]] = (for (rel <- problem.inputRels)
                                                   yield {
                                                     val tuples = problem.edb.filter(_._1 == rel).map(_._2)
                                                     rel -> tuples.foldLeft(Instance(rel)) { case (inst, t) =>
                                                       inst + (t -> vs.One)
                                                     }
                                                   }).toMap
    val edb: Config[FValue] = Config(edbMap)
    val idb = TrieSemiEvaluator(problem.rules, edb)
    val idbStr = problem.outputRels.flatMap(rel =>
      idb(rel).support.toSeq.sortBy(-_._2.v).map { case (t, v) => s"  ${v.v}: ${rel.name}$t" }
    ).mkString("," + System.lineSeparator())

    println("IDB {")
    println(idbStr)
    println("}")
  }

  def learn(): Unit = {
    val tgtLoss = args(1).toDouble
    val maxIters = args(2).toInt
    val testFile = args(3)

    require(0 <= tgtLoss && tgtLoss <= 1.0, s"Expected target loss between 0.0 and 1.0: Found $tgtLoss")
    require(maxIters > 0, s"Expected maxIters > 0: Found $maxIters")

    val learner = new Learner(problem)
    val result = learner.learn(tgtLoss, maxIters)
    assert(result._4 < tgtLoss)
    learner.reinterpret

    val testProblemString = Source.fromFile(testFile).mkString
    val testParser = new Parser()
    val testProblem = testParser.parseAll(testParser.problem, testProblemString).get

    ???
  }

  if (args(0) == "eval") { eval() }
  else if (args(0) == "learn") { learn() }
  else throw new UnsupportedOperationException(s"Unrecognized command ${args(0)}")

}
