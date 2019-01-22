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

    // TODO: Separation points are not necessarily solution points! Increasing the weights of the useful tokens might
    // expose new, previously repressed derivations of unexpected tuples.
    ???
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

    val startTime = System.nanoTime()
    while (ans.size < maxIters && currLoss >= tgtLoss && grad.abs > 0 && step.abs > 0.0) {
      val oldPos = currPos

      val solutionPointOpt = simplifyIfSolutionPoint(problem, currState)
      if (solutionPointOpt.isEmpty) {
        val delta = grad.unit * currLoss / grad.abs
        currPos = (currPos - delta).clip(0.0, 1.0).clip(0.01, 0.99, currPos)
      } else {
        currPos = solutionPointOpt.get
      }
      currOut = Timers("Learner.descend: evaluator") { evaluator(problem.rules, currPos, problem.edb) }
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
