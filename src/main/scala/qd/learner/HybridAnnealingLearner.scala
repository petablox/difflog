package qd
package learner

import org.apache.commons.math3.random.{MersenneTwister, RandomGenerator}
import qd.evaluator.Evaluator
import qd.problem.Problem
import qd.util.Timers

object HybridAnnealingLearner extends Learner {

  override val toString: String = "HybridAnnealingLearner"

  override def learn(problem: Problem, evaluator: Evaluator, scorer: Scorer, tgtLoss: Double, maxIters: Int): State = {
    Timers("HybridAnnealingLearner.learn") {
      val random = new MersenneTwister()

      // 1. Start with a random initial state
      var currState = sampleState(problem, evaluator, scorer, random)
      var trace = Vector(currState)
      var stepSize = 1.0

      // 2. Repeatedly choose next state until solution is found
      //    a. Once every MCMC_FREQ iterations, choose this next state by MCMC+SimulatedAnnealing criterion
      //    b. Otherwise, choose next state by performing a conventional gradient descent step
      val MCMC_FREQ = 20
      var iteration = 0
      while (trace.size < maxIters && currState.loss >= tgtLoss) {
        if (iteration % MCMC_FREQ == 0) {
          currState = nextStateMCMC(problem, evaluator, scorer, currState, random, iteration / MCMC_FREQ)
          stepSize = 1.0
        } else {
          val oldState = currState
          currState = NewtonRootLearner.nextState(problem, evaluator, scorer, currState)
          stepSize = (currState.pos - oldState.pos).abs
        }
        trace = trace :+ currState
        iteration = iteration + 1

        // scribe.debug(s"  currState.grad: ${currState.grad}")
        scribe.info(s"  ${currState.loss}, ${trace.map(_.loss).min}, ${currState.pos.abs}, " +
                    s"${currState.grad.abs}, $stepSize")
      }
      scribe.info(s"#Iterations: ${trace.size}.")

      val bestState = trace.minBy(_.loss)
      reinterpret(problem, evaluator, scorer, bestState)
    }
  }

  def nextStateMCMC(
                     problem: Problem,
                     evaluator: Evaluator,
                     scorer: Scorer,
                     currState: State,
                     random: RandomGenerator,
                     iteration: Int
                   ): State = {
    require(iteration >= 0)
    val solutionPointOpt = simplifyIfSolutionPoint(problem, evaluator, scorer, currState)
    solutionPointOpt.getOrElse {
      val proposedState = sampleState(problem, evaluator, scorer, random)

      val c = 1.0e-2
      val k0 = 5.0
      val temperature = 1.0 / (c * Math.log(k0 + iteration))
      def pi(negativeLoss: Double): Double = Math.exp(negativeLoss / temperature)

      val piCurr = pi(-currState.loss)
      val piProposed = pi(-proposedState.loss)
      val probAccept = Math.min(1.0, piProposed / piCurr)
      val coin = random.nextDouble()

      scribe.info(s"  c: $c, k0: $k0, iteration: $iteration, temperature: $temperature")
      scribe.info(s"  currState.loss: ${currState.loss}, piCurr: $piCurr")
      scribe.info(s"  proposedState.loss: ${proposedState.loss}, piProposed: $piProposed")
      scribe.info(s"  probAccept: $probAccept, coin: $coin")

      if (coin < probAccept) {
        scribe.info("  Accepted MCMC sample")
        proposedState
      } else {
        scribe.info("  Rejected MCMC sample")
        currState
      }
    }
  }

}
