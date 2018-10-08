package qd

import qd.evaluator.Evaluator
import qd.problem.{Problem, QDParser}

import scala.io.Source

object Main extends App {

  scribe.info(s"Hello! Difflog invoked with arguments ${args.mkString("[", ", ", "]")}")

  def readProblem(filename: String): Problem = {
    val inputString = Source.fromFile(filename).mkString
    val parser = new QDParser()
    parser.parseAll(parser.problem, inputString).get
  }

  args match {
    case Array("eval", _*) =>
      val queryFilename = args(1)
      val evaluator = Evaluator.STD_EVALUATORS(if (2 < args.length) args(2) else "trie")

      val query = readProblem(queryFilename)
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

    case Array("learn", _*) => ???
    case Array("tab2", _*) => ???
    case Array("ntp-learn", _*) => ???
    case Array("ntp-query", _*)=> ???
    case _ =>
      println(
        """Usage:
          |
          |  1. eval query.qd
          |          [[trie] | trie-semi | naive | seminaive]
          |     Evaluates the query query.qd using the specified evaluator
          |
          |  2. learn problem.qd
          |           [[trie] | trie-semi | naive | seminaive]
          |           [[xentropy] | loglinear | l2]
          |           tgtLoss [= 0.01]
          |           maxIters [= 1000]
          |     Solves the query synthesis problem described in problem.qd
          |
          |  3. tab2 problem.qd
          |          test.qd
          |          [[trie] | trie-semi | naive | seminaive]
          |          [[xentropy] | loglinear | l2]
          |          tgtLoss [= 0.01]
          |          maxIters [= 1000]
          |     Produces the statistics needed for Table 2 of the Difflog paper
        """.stripMargin)
  }

}
