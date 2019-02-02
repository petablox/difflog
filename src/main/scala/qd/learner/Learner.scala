package qd
package learner

import org.apache.commons.math3.random.RandomGenerator
import qd.evaluator.Evaluator
import qd.instance.Config
import qd.problem.Problem
import qd.tokenvec.TokenVec
import qd.util.Timers

case class State(pos: TokenVec, cOut: Config[FValue], grad: TokenVec, loss: Double)

object State {
  def apply(problem: Problem, evaluator: Evaluator, scorer: Scorer, pos: TokenVec): State = {
    State(problem, evaluator, scorer, pos, problem.edb)
  }

  def apply(problem: Problem, evaluator: Evaluator, scorer: Scorer, pos: TokenVec, oldCOut: Config[FValue]): State = {
    val cOut = Timers("State.apply: evaluator") {
      val repositionedIDB = Config.reposition(oldCOut, pos)
      evaluator(problem.rules, pos, repositionedIDB)
    }
    val grad = scorer.gradientLoss(pos, cOut, problem.idb, problem.outputRels)
    val loss = scorer.loss(cOut, problem.idb, problem.outputRels)
    State(pos, cOut, grad, loss)
  }
}

abstract class Learner {

  override def toString: String

  def learn(problem: Problem, evaluator: Evaluator, scorer: Scorer, tgtLoss: Double, maxIters: Int): State

  def sampleState(
                   problem: Problem,
                   evaluator: Evaluator,
                   scorer: Scorer,
                   random: RandomGenerator,
                   oldCOut: Config[FValue]
                 ): State = {
    val initialPos = TokenVec(problem.allTokens.map(token => token -> (0.25 + random.nextDouble() / 2)).toMap)
    State(problem, evaluator, scorer, initialPos, oldCOut)
  }

  def sampleState(problem: Problem, evaluator: Evaluator, scorer: Scorer, random: RandomGenerator): State = {
    sampleState(problem, evaluator, scorer, random, problem.edb)
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
      val newState = State(problem, evaluator, scorer, newPos, state.cOut)
      if (newState.loss <= 0.0) {
        scribe.info("... and also a solution point.")
        Some(newState)
      } else {
        scribe.info(s"... but not a solution point (newLoss = ${newState.loss}). Not terminating!")
        None
      }
    } else None
  }

  def reinterpret(problem: Problem, evaluator: Evaluator, scorer: Scorer, state: State): State = {
    val usefulTokens = (for ((rel, refOut) <- problem.discreteIDB.toSeq;
                             tuple <- refOut;
                             token <- state.cOut(rel)(tuple).l.tokenSet)
                        yield token).toSet
    val grayTokens = (for ((rel, refOut) <- state.cOut.map if problem.outputRels.contains(rel);
                           (tuple, value) <- refOut.support if !problem.discreteIDB(rel).contains(tuple);
                           token <- value.l.tokenSet)
                      yield token).toSet
    val exclusivelyUsefulTokens = usefulTokens -- grayTokens

    val newPos = TokenVec(problem.allTokens, token => if (exclusivelyUsefulTokens.contains(token)) 1.0
                                                      else if (grayTokens.contains(token)) state.pos(token).v
                                                      else 0.0)
    State(problem, evaluator, scorer, newPos, state.cOut)
  }

}

object Learner {
  val STD_LEARNERS: Map[String, Learner] =
    Set(NewtonRootLearner, HybridAnnealingLearner).map(learner => learner.toString -> learner).toMap
}