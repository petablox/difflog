package qd
package learner

import qd.evaluator.Evaluator
import qd.problem.Problem
import qd.tokenvec.TokenVec
import qd.util.Timers

object NewtonRootLearner extends Learner {

  override val toString: String = "NewtonRootLearner"

  override def learn(problem: Problem, evaluator: Evaluator, scorer: Scorer, tgtLoss: Double, maxIters: Int): State = {
    Timers("Learner.learn") {
      val trace = descend(problem, evaluator, scorer, tgtLoss, maxIters)
      val bestState = trace.minBy(_.loss)
      reinterpret(problem, evaluator, scorer, bestState)
    }
  }

  def descend(problem: Problem, evaluator: Evaluator, scorer: Scorer, tgtLoss: Double, maxIters: Int): Vector[State] = {
    val random = new scala.util.Random()

    var currState = {
      val initialPos = TokenVec(problem.allTokens.map(token => token -> (0.25 + random.nextDouble() / 2)).toMap)
      State(problem, evaluator, scorer, initialPos)
    }

    var ans = Vector(currState)
    var step = ans(0).pos

    while (ans.size < maxIters && currState.loss >= tgtLoss && currState.grad.abs > 0 && step.abs > 0.0) {
      val oldState = currState
      currState = nextState(problem, evaluator, scorer, currState)
      step = currState.pos - oldState.pos
      ans = ans :+ currState
      // scribe.debug(s"  currState.grad: ${currState.grad}")
      scribe.info(s"  ${currState.loss}, ${ans.map(_.loss).min}, ${currState.pos.abs}, ${currState.grad.abs}, ${step.abs}")
    }
    scribe.info(s"#Iterations: ${ans.size}.")

    ans
  }

  def nextState(problem: Problem, evaluator: Evaluator, scorer: Scorer, currState: State): State = {
    val State(currPos, _, currGrad, currLoss) = currState
    val solutionPointOpt = simplifyIfSolutionPoint(problem, evaluator, scorer, currState)
    solutionPointOpt.getOrElse {
      val delta = currGrad.unit * currLoss / currGrad.abs
      val nextPos = (currPos - delta).clip(0.0, 1.0).clip(0.01, 0.99, currPos)
      State(problem, evaluator, scorer, nextPos)
    }
  }

}
