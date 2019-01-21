package qd
package learner

import qd.Semiring.FValueSemiringObj
import qd.evaluator.Evaluator
import qd.learner.Learner.{State, keepHighestTokens, keepUsefulRules, reinterpret}
import qd.problem.Problem
import qd.tokenvec.TokenVec

object LineLearner {

  def learn(problem: Problem, evaluator: Evaluator, scorer: Scorer, tgtLoss: Double, maxIters: Int): State = {
    val trace = descend(problem, evaluator, scorer, tgtLoss, maxIters)
    val bestState = trace.minBy(_.loss)
    val highTrace = keepHighestTokens(problem, evaluator, scorer, bestState)
    val usefulTrace = highTrace.map(state => keepUsefulRules(problem, evaluator, scorer, state))
    val reinterpretTrace = usefulTrace.map(state => reinterpret(problem, evaluator, scorer, state))
    reinterpretTrace.minBy(_.loss)
  }

  def simplifyIfSolutionPoint(problem: Problem, state: State): Option[TokenVec] = {
    val usefulTokens = (for ((rel, refOut) <- problem.discreteIDB.toSeq;
                             tuple <- refOut;
                             token <- state.cOut(rel)(tuple).l.tokenSet)
                        yield token).toSet
    val eliminableTokens = problem.allTokens -- usefulTokens

    val isSolutionPoint = problem.outputRels.forall { relation =>
      val expectedTuples = problem.discreteIDB(relation)
      val unexpectedTuples = state.cOut(relation).support.filterNot { case (t, _) => expectedTuples.contains(t) }
      unexpectedTuples.forall { case (_, FValue(_, l)) => (l.tokenSet & eliminableTokens).nonEmpty }
    }

    if (isSolutionPoint) Some(TokenVec(problem.allTokens, token => if (usefulTokens.contains(token)) 1.0 else 0.0))
    else None
  }

  def descend(problem: Problem, evaluator: Evaluator, scorer: Scorer, tgtLoss: Double, maxIters: Int): Vector[State] = {
    val random = new scala.util.Random()

    var currPos = TokenVec(problem.allTokens.map(token => token -> (0.25 + random.nextDouble() / 0.5)).toMap)
    var currOut = evaluator(problem.rules, currPos, problem.edb)
    var currLoss = scorer.loss(currOut, problem.idb, problem.outputRels)
    var currState = State(currPos, currOut, currLoss)
    var ans = Vector(currState)

    var grad = scorer.gradientLoss(currPos, currOut, problem.idb, problem.outputRels)
    var step = currPos

    val startTime = System.nanoTime()
    while (ans.size < maxIters && currLoss >= tgtLoss && grad.abs > 0 && step.abs > 0.0) {
      val oldPos = currPos

      val delta = grad.unit * currLoss / grad.abs
      currPos = (currPos - delta).clip(0.0, 1.0).clip(0.01, 0.99, currPos)
      currOut = evaluator(problem.rules, currPos, problem.edb)
      currLoss = scorer.loss(currOut, problem.idb, problem.outputRels)
      currState = State(currPos, currOut, currLoss)

      ans = ans :+ currState
      grad = scorer.gradientLoss(currPos, currOut, problem.idb, problem.outputRels)
      step = currPos - oldPos

      // scribe.debug(s"  grad: $grad")
      scribe.info(s"  $currLoss, ${ans.map(_.loss).min}, ${currPos.abs}, ${grad.abs}, ${step.abs}")
    }
    val endTime = System.nanoTime()
    val timePerIter = (endTime - startTime) / 1.0e9 / ans.size
    scribe.info(s"#Iterations: ${ans.size}.")
    scribe.info(s"Time / iteration: $timePerIter seconds.")

    ans
  }

}
