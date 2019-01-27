package qd
package learner

import org.apache.commons.math3.random.MersenneTwister
import qd.evaluator.Evaluator
import qd.problem.Problem
import qd.util.Timers

object NewtonRootLearner extends Learner {

  override val toString: String = "NewtonRootLearner"

  override def learn(problem: Problem, evaluator: Evaluator, scorer: Scorer, tgtLoss: Double, maxIters: Int): State = {
    Timers("NewtonRootLearner.learn") {
      var currState = sampleState(problem, evaluator, scorer, new MersenneTwister())
      var trace = Vector(currState)
      var stepSize = 1.0

      while (trace.size < maxIters && currState.loss >= tgtLoss && currState.grad.abs > 0 && stepSize > 0.0) {
        val oldState = currState
        currState = nextState(problem, evaluator, scorer, currState)
        stepSize = (currState.pos - oldState.pos).abs
        trace = trace :+ currState
        // scribe.debug(s"  currState.grad: ${currState.grad}")
        scribe.info(s"  ${currState.loss}, ${trace.map(_.loss).min}, ${currState.pos.abs}, " +
                    s"${currState.grad.abs}, $stepSize")
      }
      scribe.info(s"#Iterations: ${trace.size}.")

      val bestState = trace.minBy(_.loss)
      reinterpret(problem, evaluator, scorer, bestState)
    }
  }

  def nextState(problem: Problem, evaluator: Evaluator, scorer: Scorer, currState: State): State = {
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
      State(problem, evaluator, scorer, nextPos)
    }
  }

}
