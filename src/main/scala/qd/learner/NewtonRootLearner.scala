package qd
package learner

import scala.math.{min, max}
import scala.util.Random
import qd.evaluator.Evaluator
import qd.problem.Problem
import qd.tokenvec.TokenVec
import qd.util.Timers

object NewtonRootLearner extends Learner {

  override val toString: String = "NewtonRootLearner"

  override def learn(problem: Problem, evaluator: Evaluator, scorer: Scorer, tgtLoss: Double, maxIters: Int): State = {
    Timers("NewtonRootLearner.learn") {
      var currState = sampleState(problem, evaluator, scorer)
      var bestState = currState
      var stepSize = 1.0

      var numIters = 0
      while (numIters < maxIters && currState.loss >= tgtLoss && currState.grad.abs > 0 && stepSize > 0.0) {
        val oldState = currState
        currState = nextState(problem, evaluator, scorer, currState, Set())
        stepSize = (currState.pos - oldState.pos).abs

        if (currState.loss < bestState.loss) {
          bestState = currState
        }
        numIters = numIters + 1

        // scribe.debug(s"  currState.grad: ${currState.grad}")
        scribe.info(s"  ${currState.loss}, ${bestState.loss}, ${currState.pos.abs}, " +
                    s"${currState.grad.abs}, $stepSize")
      }
      scribe.info(s"#Iterations: $numIters.")

      reinterpret(problem, evaluator, scorer, bestState)
    }
  }

  def nextState(problem: Problem, evaluator: Evaluator, scorer: Scorer, currState: State, forbiddenTokens: Set[Token]): State = {
    val solutionPointOpt = simplifyIfSolutionPoint(problem, evaluator, scorer, currState)
    solutionPointOpt.getOrElse {
      val State(currPos, _, currGrad, currLoss) = currState

      /* // Repeatedly lower delta until the step is actually heading downwards
      var delta = currGrad.unit * currLoss / currGrad.abs
      var nextPos = (currPos - delta).clip(0.0, 1.0).clip(0.01, 0.99, currPos)
      var nextState = State(problem, evaluator, scorer, nextPos)

      while (nextState.loss > currState.loss) {
        scribe.info(s"  Halving delta! ${delta.abs}")
        delta = delta * 0.5
        nextPos = (currPos - delta).clip(0.0, 1.0).clip(0.01, 0.99, currPos)
        nextState = State(problem, evaluator, scorer, nextPos)
      }

      nextState */

      val delta = currGrad.unit * currLoss / currGrad.abs
      val nextPos = (currPos - delta).clip(0.0, 1.0).clip(0.01, 0.99, currPos)
      val visibleTokens = for (rel <- problem.outputRels; (_, v) <- currState.cOut(rel).support; token <- v.l.toVector)
                          yield token
      val newPos = TokenVec(problem.pos.keySet, token =>
          if (forbiddenTokens.contains(token)) 0.0
          else if (visibleTokens.contains(token)) nextPos(token).v
          else max(min (nextPos(token).v + (new Random).nextDouble() / 10 - 0.04, 0.99), 0.01))
      State(problem, evaluator, scorer, newPos, currState.cOut)
    }
  }

}
