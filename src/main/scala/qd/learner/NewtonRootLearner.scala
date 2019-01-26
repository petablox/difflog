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
      reinterpret(problem, bestState)
    }
  }

  def descend(problem: Problem, evaluator: Evaluator, scorer: Scorer, tgtLoss: Double, maxIters: Int): Vector[State] = {
    val random = new scala.util.Random()

    var currPos = TokenVec(problem.allTokens.map(token => token -> (0.25 + random.nextDouble() / 2)).toMap)
    var currOut = evaluator(problem.rules, currPos, problem.edb)
    var currLoss = scorer.loss(currOut, problem.idb, problem.outputRels)
    var currState = State(currPos, currOut, currLoss)
    var ans = Vector(currState)

    var grad = scorer.gradientLoss(currPos, currOut, problem.idb, problem.outputRels)
    var step = currPos

    while (ans.size < maxIters && currLoss >= tgtLoss && grad.abs > 0 && step.abs > 0.0) {
      val oldPos = currPos

      val solutionPointOpt = simplifyIfSolutionPoint(problem, evaluator, scorer, currState)
      if (solutionPointOpt.nonEmpty) {
        currState = solutionPointOpt.get
        currPos = currState.pos
        currOut = currState.cOut
        currLoss = currState.loss
      } else {
        val delta = grad.unit * currLoss / grad.abs
        currPos = (currPos - delta).clip(0.0, 1.0).clip(0.01, 0.99, currPos)
        currOut = Timers("Learner.descend: evaluator") { evaluator(problem.rules, currPos, problem.edb) }
        currLoss = scorer.loss(currOut, problem.idb, problem.outputRels)
        currState = State(currPos, currOut, currLoss)
      }

      ans = ans :+ currState
      grad = scorer.gradientLoss(currPos, currOut, problem.idb, problem.outputRels)
      step = currPos - oldPos

      // scribe.debug(s"  grad: $grad")
      scribe.info(s"  $currLoss, ${ans.map(_.loss).min}, ${currPos.abs}, ${grad.abs}, ${step.abs}")
    }
    scribe.info(s"#Iterations: ${ans.size}.")

    ans
  }

}
