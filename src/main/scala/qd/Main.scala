package qd

import qd.evaluator.{Evaluator, NaiveEvaluator, SeminaiveEvaluator, TrieEvaluator}
import qd.problem.Problem._
import qd.problem.{Problem, QDParser}

import scala.io.Source

object Main extends App {

  scribe.info(s"Hello! Difflog invoked with arguments ${args.mkString("[", ", ", "]")}")

  def readProblem(filename: String): Problem = {
    val inputString = Source.fromFile(filename).mkString
    val parser = new QDParser()
    parser.parseAll(parser.problem, inputString).get
  }

  def getEvaluator(name: String): Evaluator = name match {
    case "trie" => TrieEvaluator
    case "semi" => SeminaiveEvaluator
    case "naive" => NaiveEvaluator
    case _ =>
      scribe.warn(s"Unable to find evaluator $name")
      TrieEvaluator
  }

  def getNormMode(name: String): NormMode = name match {
    case "auto" => AUTO_NORM
    case "norm" => DO_NORM
    case "nonorm" => NO_NORM
  }

  args match {
    case Array("eval", _*) =>
      val queryFilename = args(1)
      val evaluator = getEvaluator(if (2 < args.length) args(2) else "trie")
      val normMode = getNormMode(if (3 < args.length) args(3) else "auto")

      val query = readProblem(queryFilename)
      val idb = evaluator(query.rules, normMode, query.edb)
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

    case Array("learn", _*) => ???
    case Array("tab2", _*) => ???
    case Array("ntp-learn", _*) => ???
    case Array("ntp-query", _*)=> ???
    case _ =>
      println(
        """Usage:
          |
          |  1. eval query.qd
          |          [[trie] | semi | naive]
          |          [[auto] | norm | nonorm]
          |     Evaluates the query query.qd, using the specified evaluator and query normalization setting
          |
          |  2. learn problem.qd
          |           [[trie] | semi | naive]
          |           [[auto] | norm | nonorm]
          |           [[xentropy] | loglinear | l2]
          |           tgtLoss [= 0.01]
          |           maxIters [= 1000]
          |     Solves the query synthesis problem described in problem.qd
          |
          |  3. tab2 problem.qd
          |          test.qd
          |          [[trie] | semi]
          |          [[auto] | norm | nonorm]
          |          [[xentropy] | loglinear | l2]
          |          tgtLoss [= 0.01]
          |          maxIters [= 1000]
          |     Produces the statistics needed for Table 2 of the Difflog paper
        """.stripMargin)
  }

}
