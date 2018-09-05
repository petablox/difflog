package qd

import qd.Semiring.FValueSemiringObj
import qd.evaluator.{TrieEvaluator, TrieSemiEvaluator}
import qd.learner.{L2Scorer, Learner}

import scala.io.Source

object Main extends App {

  implicit val vs: FValueSemiring = FValueSemiringObj

  def readProblem(filename: String): Problem = {
    val inputString = Source.fromFile(filename).mkString
    val parser = new Parser()
    parser.parseAll(parser.problem, inputString).get
  }

  def eval(): Unit = {
    val problem = readProblem(args(1))
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
    val (trainFile, tgtLoss, maxIters, testFile) = (args(1), args(2).toDouble, args(3).toInt, args(4))

    require(0 <= tgtLoss && tgtLoss <= 1.0, s"Expected target loss between 0.0 and 1.0: Found $tgtLoss")
    require(maxIters > 0, s"Expected maxIters > 0: Found $maxIters")

    val trainProblem = readProblem(trainFile)
    val learner = new Learner(trainProblem)
    val result = learner.learn(tgtLoss, maxIters)
    assert(result._4 < tgtLoss)
    learner.reinterpret

    val testProblemString = Source.fromFile(testFile).mkString
    val testParser = new Parser()
    val testProblem = testParser.parseAll(testParser.problem, testProblemString).get

    ???
  }

  def tab2(): Unit = {
    val (trainFile, tgtLoss, maxIters, testFile) = (args(1), args(2).toDouble, args(3).toInt, args(4))

    require(0 <= tgtLoss && tgtLoss <= 1.0, s"Expected target loss between 0.0 and 1.0: Found $tgtLoss")
    require(maxIters > 0, s"Expected maxIters > 0: Found $maxIters")

    val trainProblem = readProblem(trainFile)
    val learner = new Learner(trainProblem)
    learner.learn(tgtLoss, maxIters)
    // learner.keepUseful
    val reinterpretedResult = learner.reinterpret

    val testProblem = readProblem(testFile)
    val testScorer = new L2Scorer(testProblem.edbConfig, testProblem.idbConfig, TrieEvaluator)
    for ((_, prog, _, trainLoss) <- reinterpretedResult) {
      val testIDB = TrieEvaluator(prog, testProblem.edbConfig)
      val testLoss = testScorer.loss(testIDB)
      println(s"${prog.name} $trainLoss $testLoss")

      /* if (trainLoss == 0 && testLoss > 0) {
        for (rel <- testScorer.outputRels;
             allTuples = testScorer.refIDB(rel).support.map(_._1) ++ testIDB(rel).support.map(_._1);
             t <- allTuples;
             vRef = testScorer.refIDB(rel)(t);
             vProd = testIDB(rel)(t)
             if vRef.v != vProd.v) {
          println(s"$rel $t $vRef $vProd")
        }
      } */
    }
  }

  if (args(0) == "eval") { eval() }
  else if (args(0) == "learn") { learn() }
  else if (args(0) == "tab2") { tab2() }
  else throw new UnsupportedOperationException(s"Unrecognized command ${args(0)}")

}
