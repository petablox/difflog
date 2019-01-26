package qd
package learner

import qd.evaluator.Evaluator
import qd.instance.Config
import qd.problem.Problem
import qd.tokenvec.TokenVec
import qd.util.Timers

case class State(pos: TokenVec, cOut: Config[FValue], loss: Double)

abstract class Learner {

  override def toString: String

  def learn(problem: Problem, evaluator: Evaluator, scorer: Scorer, tgtLoss: Double, maxIters: Int): State

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

}

object Learner {
  val STD_LEARNERS: Map[String, Learner] = Set(NewtonRootLearner).map(learner => learner.toString -> learner).toMap
}