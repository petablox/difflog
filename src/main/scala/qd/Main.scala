package qd

import qd.evaluator.Evaluator
import qd.learner.{Learner, Scorer}
import qd.problem.{ALPSParser, Problem, QDParser}

import scala.io.Source

object Main extends App {

  scribe.info(s"Hello! Difflog invoked with arguments ${args.mkString("[", ", ", "]")}")

  def readQDProblem(filename: String): Problem = {
    val inputString = Source.fromFile(filename).mkString
    new QDParser().parse(inputString)
  }

  def readALPSProblem(dataFilename: String, templateFilename: String): Problem = {
    val dataStr = Source.fromFile(dataFilename).mkString
    val templateStr = Source.fromFile(templateFilename).mkString
    new ALPSParser().parse(dataStr, templateStr)
  }

  args match {
    case Array("eval", queryFilename, evaluatorName) =>
      val query = readQDProblem(queryFilename)
      val evaluator = Evaluator.STD_EVALUATORS(evaluatorName)

      val idb = evaluator(query.rules, query.edb)
      for (rel <- query.outputRels) {
        for ((t, v) <- idb(rel).support.toSeq.sortBy(-_._2.v)) {
          println(s"$v: ${rel.name}$t")
          scribe.trace(s"$v: ${rel.name}$t")
        }
      }
      for (rel <- query.inventedRels) {
        for ((t, v) <- idb(rel).support.toSeq.sortBy(-_._2.v)) {
          scribe.trace(s"$v: ${rel.name}$t")
        }
      }

    case Array("learn", queryFilenameTrain, evaluatorName, scorerName, tgtLossStr, maxItersStr) =>
      val queryTrain = readQDProblem(queryFilenameTrain)
      val evaluator = Evaluator.STD_EVALUATORS(evaluatorName)
      val scorerFactory = Scorer.STD_SCORERS(scorerName)
      val tgtLoss = tgtLossStr.toDouble
      val maxIters = maxItersStr.toInt
      require(maxIters > 0)

      val learner = new Learner(queryTrain, evaluator, scorerFactory)
      learner.learn(tgtLoss, maxIters)
      learner.reinterpret()
      ???

    case Array("tab2", _*) => ???
    case Array("alps", dataFilename, templateFilename, evaluatorName, scorerName, tgtLossStr, maxItersStr) =>
      val query = readALPSProblem(dataFilename, templateFilename)
      val evaluator = Evaluator.STD_EVALUATORS(evaluatorName)
      val scorerFactory = Scorer.STD_SCORERS(scorerName)
      val tgtLoss = tgtLossStr.toDouble
      val maxIters = maxItersStr.toInt
      require(maxIters > 0)

      val learner = new Learner(query, evaluator, scorerFactory)
      learner.learn(tgtLoss, maxIters)
      learner.reinterpret()
      ???

    case Array("ntp-learn", _*) => ???
    case Array("ntp-query", _*)=> ???
    case _ =>
      println(
        """Usage:
          |
          |  1. eval query.qd
          |          [trie | trie-semi | naive | seminaive]
          |     Evaluates the query query.qd using the specified evaluator
          |
          |  2. learn problem.qd
          |           [trie | trie-semi | naive | seminaive]
          |           [xentropy | l2]
          |           tgtLoss
          |           maxIters
          |     Solves the query synthesis problem described in problem.qd
          |
          |  3. tab2 problem.qd
          |          test.qd
          |          [trie | trie-semi | naive | seminaive]
          |          [xentropy | l2]
          |          tgtLoss
          |          maxIters
          |     Produces the statistics needed for Table 2 of the Difflog paper
          |
          |  4. alps data.d
          |          templates.tp
          |          [trie | trie-semi | naive | seminaive]
          |          [xentropy | l2]
          |          tgtLoss
          |          maxIters
          |     Runs Difflog in the ALPS setting
        """.stripMargin)
  }

}
