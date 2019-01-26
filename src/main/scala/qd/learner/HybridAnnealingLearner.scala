package qd
package learner

import qd.evaluator.Evaluator
import qd.learner.NewtonRootLearner.{nextState, sampleState}
import qd.problem.Problem
import qd.util.Timers

import scala.util.Random

object HybridAnnealingLearner extends Learner {

  override val toString: String = "HybridAnnealing"

  override def learn(problem: Problem, evaluator: Evaluator, scorer: Scorer, tgtLoss: Double, maxIters: Int): State = {
    Timers("HybridAnnealingLearner.learn") {
      val trace = descend(problem, evaluator, scorer, tgtLoss, maxIters)
      val bestState = trace.minBy(_.loss)
      reinterpret(problem, evaluator, scorer, bestState)
    }
  }

  def descend(problem: Problem, evaluator: Evaluator, scorer: Scorer, tgtLoss: Double, maxIters: Int): Vector[State] = {
    val random = new Random()

    // Start with a random initial state
    var currState = sampleState(problem, evaluator, scorer, random)
    var ans = Vector(currState)
    var step = ans(0).pos

    var i = 0
    // Repeat until solution found
    while (ans.size < maxIters && currState.loss >= tgtLoss) {
      // 1. Perform conventional gradient descent for k = 20 steps
      for (j <- Range(0, 20) if ans.size < maxIters &&
                                currState.loss >= tgtLoss &&
                                currState.grad.abs > 0 &&
                                step.abs > 0.0) {
        val oldState = currState
        currState = nextState(problem, evaluator, scorer, currState)
        step = currState.pos - oldState.pos
        ans = ans :+ currState
        // scribe.debug(s"  currState.grad: ${currState.grad}")
        scribe.info(s"  ${currState.loss}, ${ans.map(_.loss).min}, ${currState.pos.abs}, ${currState.grad.abs}, ${step.abs}")
      }

      // 2. Sample a new state, and accept based on the MCMC transition probability
      if (ans.size < maxIters && currState.loss >= tgtLoss) {
        val proposedState = sampleState(problem, evaluator, scorer, random)
        if (random.nextDouble() < probAccept(currState, proposedState, i)) {
          val oldState = currState
          currState = nextState(problem, evaluator, scorer, currState)
          step = currState.pos - oldState.pos
          ans = ans :+ currState
          scribe.info("  Accepted MCMC sample")
          // scribe.debug(s"  currState.grad: ${currState.grad}")
          scribe.info(s"  ${currState.loss}, ${ans.map(_.loss).min}, ${currState.pos.abs}, ${currState.grad.abs}, ${step.abs}")
        } else {
          scribe.info("  Rejected MCMC sample")
        }
        i = i + 1
      }
    }
    scribe.info(s"#Iterations: ${ans.size}.")

    ans
  }

  def probAccept(currState: State, nextState: State, iteration: Int): Double = {
    val C = 3.0
    val k0 = 5.0
    val temperature = 1.0 / C / Math.log(k0 + iteration)
    def pi(negativeLoss: Double): Double = Math.exp(negativeLoss / temperature)
    val piCurr = pi(-currState.loss)
    val piNext = pi(-nextState.loss)
    val ans = Math.min(1.0, piNext / piCurr)
    scribe.info(s"  C: $C, k0: $k0, iteration: $iteration, temperature: $temperature, piCurr: $piCurr, piNext: $piNext, ans: $ans")
    ans
  }

}
