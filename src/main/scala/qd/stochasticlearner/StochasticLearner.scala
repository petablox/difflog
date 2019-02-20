package qd
package learner

import qd.dgraph.{Derivation, Extractor}
import qd.dgraph.Derivation.DGraph
import qd.problem.Problem
import qd.tokenvec.TokenVec
import qd.util.Contract

case class StochasticState(pos: TokenVec, grad: TokenVec, loss: Double)

object StochasticState {
  def apply(problem: Problem, pos: TokenVec): StochasticState = {
    StochasticState(problem, pos, problem.edb)
  }

  def apply(problem: Problem, scorer: StochasticScorer, pos: TokenVec): State = {
    val grad = scorer.gradientLoss(pos, cOut, problem.idb, problem.outputRels)
    val loss = scorer.loss(cOut, problem.idb, problem.outputRels)
    State(pos, cOut, grad, loss)
  }
}


object StochasticLearner {

  def learn(problem: Problem, extractor: Extractor, scorer: Scorer, nsamples: Int, tgtLoss: Double, maxIters: Int): State = {
    Contract.require(nsamples > 0)

    val edbGraph = problem.discreteEDB.transform({ case (_, tuples) =>
      tuples.map(t => t -> Set(Derivation(t))).toMap
    })
    val idbGraph = extractor.apply(problem.rules, edbGraph)

    var currState = sampleState(problem, evaluator, scorer)
    var bestState = currState
    var stepSize = 1.0


    val usefulTokens = (for ((rel, refOut) <- problem.discreteIDB.toSeq;
                             tuple <- refOut;
                             token <- state.cOut(rel)(tuple).l.tokenSet)
      yield token).toSet

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

    ???
  }

  def sampleState(
                   problem: Problem,
                   scorer: Scorer,
                   oldCOut: Config[FValue]
                 ): StochasticState = {
    val initialPos = TokenVec(problem.allTokens.map(token => token -> Random.nextDouble(0.25, 0.75)).toMap)
    StochasticState(problem,  scorer, initialPos, oldCOut)
  }

  def sampleState(problem: Problem, evaluator: Evaluator, scorer: Scorer): StochasticState = {
    sampleState(problem, evaluator, scorer, problem.edb)
  }

  def simplifyIfSolutionPoint(problem: Problem, evaluator: Evaluator, scorer: Scorer, state: State): Option[StochasticState] = {
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

  def reinterpret(problem: Problem,  state: State): State = {
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
    State(problem, newPos, state.cOut)
  }

  def nextState(problem: Problem,  scorer: Scorer, currState: State, forbiddenTokens: Set[Token]): State = {
    val solutionPointOpt = simplifyIfSolutionPoint(problem, currState)
    solutionPointOpt.getOrElse {
      val State(currPos, _, currGrad, currLoss) = currState


      val delta = currGrad.unit * currLoss / currGrad.abs
      val nextPos = (currPos - delta).clip(0.0, 1.0).clip(0.01, 0.99, currPos)
      val newPos = TokenVec(problem.pos.keySet, token =>
        if (forbiddenTokens.contains(token)) 0.0
        else nextPos(token).v)
      State(problem, newPos, currState.cOut)
    }
  }

}