package qd

import qd.Semiring.FValueSemiringObj
import qd.evaluator.Evaluator
import qd.learner.{NewtonRootLearner, Scorer}
import qd.problem.{ALPSParser, Problem, QDParser}
import qd.util.Timers.Timer
import qd.util.{Contract, Counters, Timers}

import scala.io.Source

object Main extends App {

  def readQDProblem(filename: String): Problem = {
    val inputString = Source.fromFile(filename).mkString
    new QDParser().parse(inputString)
  }

  def readALPSProblem(dataFilename: String, templateFilename: String): Problem = {
    val dataStr = Source.fromFile(dataFilename).mkString
    val templateStr = Source.fromFile(templateFilename).mkString
    ALPSParser.parse(dataStr, templateStr)
  }

  Timers("Main") {

    scribe.info(s"Hello! Difflog invoked with arguments ${args.mkString("[", ", ", "]")}")

    args match {
      case Array("eval", queryFilename, evaluatorName) =>
        val query = readQDProblem(queryFilename)
        val evaluator = Evaluator.STD_EVALUATORS(evaluatorName)

        val idb = evaluator(query.rules, query.pos, query.edb)
        for (rel <- query.outputRels) {
          for ((t, v) <- idb(rel).support.sortBy(-_._2.v)) {
            println(s"$v: ${rel.name}$t")
            scribe.trace(s"$v: ${rel.name}$t")
          }
        }
        for (rel <- query.inventedRels) {
          for ((t, v) <- idb(rel).support.sortBy(-_._2.v)) {
            scribe.trace(s"$v: ${rel.name}$t")
          }
        }

      case Array("learn", queryFilenameTrain, evaluatorName, scorerName, tgtLossStr, maxItersStr) =>
        val queryTrain = readQDProblem(queryFilenameTrain)
        val evaluator = Evaluator.STD_EVALUATORS(evaluatorName)
        val scorer = Scorer.STD_SCORERS(scorerName)
        val tgtLoss = tgtLossStr.toDouble
        val maxIters = maxItersStr.toInt
        Contract.require(maxIters > 0)

        NewtonRootLearner.learn(queryTrain, evaluator, scorer, tgtLoss, maxIters)
        ???

      case Array("tab2", _*) => ???
      case Array("alps", dataFilename, templateFilename, evaluatorName, scorerName, tgtLossStr, maxItersStr) =>
        val query = readALPSProblem(dataFilename, templateFilename)
        val evaluator = Evaluator.STD_EVALUATORS(evaluatorName)
        val scorer = Scorer.STD_SCORERS(scorerName)
        val tgtLoss = tgtLossStr.toDouble
        val maxIters = maxItersStr.toInt
        Contract.require(maxIters > 0)

        val result = NewtonRootLearner.learn(query, evaluator, scorer, tgtLoss, maxIters)
        println(s"// Achieved loss ${result.loss}")
        val weightedRules = query.rules.map(rule => (Value(rule.lineage, result.pos), rule))
        weightedRules.filter({ case (weight, _) => FValueSemiringObj.nonZero(weight) })
                     .toVector
                     .sortBy(-_._1.v)
                     .map({ case (weight, rule) => s"$weight: $rule" })
                     .foreach(println)

      case Array("ntp-learn", _*) => ???
      case Array("ntp-query", _*)=> ???
      case _ =>
        val stdEvaluatorsStr = Evaluator.STD_EVALUATORS.keys.mkString(" | ")
        val stdScorersStr = Scorer.STD_SCORERS.keys.mkString(" | ")
        println(
          s"""Usage:
             |
             |  1. eval query.qd
             |          [ $stdEvaluatorsStr ]
             |     Evaluates the query query.qd using the specified evaluator
             |
             |  2. learn problem.qd
             |           [ $stdEvaluatorsStr ]
             |           [ $stdScorersStr ]
             |           tgtLoss
             |           maxIters
             |     Solves the query synthesis problem described in problem.qd
             |
             |  3. tab2 problem.qd
             |          test.qd
             |          [ $stdEvaluatorsStr ]
             |          [ $stdScorersStr ]
             |          tgtLoss
             |          maxIters
             |     Produces the statistics needed for Table 2 of the Difflog paper
             |
             |  4. alps data.d
             |          templates.tp
             |          [ $stdEvaluatorsStr ]
             |          [ $stdScorersStr ]
             |          tgtLoss
             |          maxIters
             |     Runs Difflog in the ALPS setting
           """.stripMargin)
    }

  }

  scribe.info {
    val strs = for ((name, Timer(duration, invocations)) <- Timers.getSnapshot.toVector.sortBy(-_._2.duration))
               yield s"$name: ${duration / 1.0e9} seconds, $invocations invocations."
    if (strs.nonEmpty) "Timers:" + System.lineSeparator() + strs.mkString(System.lineSeparator())
    else "No timers enabled"
  }
  scribe.info {
    val strs = for ((name, count) <- Counters.getSnapshot.toVector.sortBy(-_._2)) yield s"$name: $count"
    if (strs.nonEmpty) "Counters:" + System.lineSeparator() + strs.mkString(System.lineSeparator())
    else "No counters enabled"
  }
  scribe.info("Bye!")

}
