package qd
package learner

import qd.evaluator.Evaluator
import qd.instance.Config
import qd.problem.Problem
import qd.tokenvec.TokenVec
import qd.util.Timers

object Learner {

  case class State(pos: TokenVec, cOut: Config[FValue], loss: Double)

  def learn(problem: Problem, evaluator: Evaluator, scorer: Scorer, tgtLoss: Double, maxIters: Int): State = {
    Timers("Learner.learn") {
      val trace = descend(problem, evaluator, scorer, tgtLoss, maxIters)
      val bestState = trace.minBy(_.loss)
      reinterpret(problem, bestState)
    }
  }

  def reinterpret(problem: Problem, state: State): State = {
    val usefulTokens = (for ((rel, refOut) <- problem.discreteIDB.toSeq;
                             tuple <- refOut;
                             token <- state.cOut(rel)(tuple).l.tokenSet)
                        yield token).toSet
    val grayTokens = (for ((rel, refOut) <- state.cOut.map if problem.outputRels.contains(rel);
                           (tuple, value) <- refOut.support if !problem.discreteIDB(rel).contains(tuple);
                           token <- value.l.tokenSet)
                      yield token).toSet
    val exclusivelyUsefulTokens = usefulTokens -- grayTokens

    val State(pos, cOut, loss) = state
    val newPos = TokenVec(problem.allTokens, token => if (exclusivelyUsefulTokens.contains(token)) 1.0
                                                      else if (grayTokens.contains(token)) pos(token).v
                                                      else 0.0)

    State(newPos, cOut, loss)
  }

  def simplifyIfSolutionPoint(problem: Problem, evaluator: Evaluator, scorer: Scorer, state: State): Option[State] = {
    val usefulTokens = (for ((rel, refOut) <- problem.discreteIDB.toSeq;
                             tuple <- refOut;
                             token <- state.cOut(rel)(tuple).l.tokenSet)
                        yield token).toSet
    val eliminableTokens = problem.allTokens -- usefulTokens

    val isSeparable = problem.outputRels.forall { relation =>
      val expectedTuples = problem.discreteIDB(relation)
      val unexpectedTuples = state.cOut(relation).support.filterNot { case (t, _) => expectedTuples.contains(t) }
      unexpectedTuples.forall { case (_, FValue(_, l)) => (l.tokenSet & eliminableTokens).nonEmpty }
    }

    if (isSeparable) {
      scribe.info("Current position is separable...")
      val newPos = TokenVec(problem.allTokens, token => if (usefulTokens.contains(token)) 1.0 else 0.0)
      val newOut = Timers("Learner.descend: evaluator") { evaluator(problem.rules, newPos, problem.edb) }
      val newLoss = scorer.loss(newOut, problem.idb, problem.outputRels)
      if (newLoss <= 0.0) {
        scribe.info("... and also a solution point.")
        Some(State(newPos, newOut, newLoss))
      } else {
        scribe.info(s"... but not a solution point (newLoss = $newLoss). Not terminating!")
        None
      }
    } else None
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
