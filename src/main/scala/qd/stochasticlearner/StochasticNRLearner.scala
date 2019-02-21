package qd
package stochasticlearner

import qd.dgraph.Derivation.DGraph
import qd.dgraph.{Derivation, Extractor}
import qd.evaluator.Evaluator
import qd.learner.Learner
import qd.problem.Problem
import qd.tokenvec.TokenVec
import qd.util.{Contract, Timers}

object StochasticNRLearner extends StochasticLearner {

  override def toString: String = "NewtonRootLearner"

  def learn(
             problem: Problem,
             extractor: Extractor,
             evaluator: Evaluator,
             scorer: StochasticScorer,
             nsamples: Int,
             tgtLoss: Double,
             maxIters: Int
           ): StochasticState = Timers("StochasticLearner.learn") {
    Contract.require(nsamples > 0)

    // 1. Extract full derivation graph
    val dgraph = Timers("StochasticLearner.learn: dgraph") {
      val edbGraph = problem.discreteEDB.transform({ case (_, tuples) =>
        tuples.map(t => t -> Set(Derivation(t))).toMap
      })
      extractor.apply(problem.rules, edbGraph)
    }

    // 2. Start with a random initial state
    var forbiddenTokens: Set[Token] = Set()
    var currState = StochasticLearner.sampleState(problem, evaluator, scorer)
    var bestState = currState
    var stepSize = 1.0

    // 3. Repeatedly choose next state until solution is found
    //    a. Do no more than numIters iterations
    //    b. Stop once target loss is achieved
    var numIters = 0
    while (numIters < maxIters && currState.loss >= tgtLoss) {
      val newlyForbiddenTokens = Learner.findForbiddenTokens(problem, currState.cOut)
      if (newlyForbiddenTokens.nonEmpty) {
        forbiddenTokens = forbiddenTokens ++ newlyForbiddenTokens
      }

      val oldState = currState
      currState = nextState(problem, dgraph, evaluator, scorer, currState, nsamples, forbiddenTokens)
      stepSize = (currState.pos - oldState.pos).abs

      if (currState.loss < bestState.loss) {
        bestState = currState
      }
      numIters = numIters + 1

      // scribe.debug(s"  currState.grad: ${currState.grad}")
      scribe.info(s"  ${currState.loss}, ${bestState.loss}, ${currState.pos.abs}, $stepSize")
    }
    scribe.info(s"#Iterations: $numIters.")

    StochasticLearner.reinterpret(problem, evaluator, scorer, bestState)
  }

  def nextState(
                 problem: Problem,
                 dgraph: DGraph,
                 evaluator: Evaluator,
                 scorer: StochasticScorer,
                 currState: StochasticState,
                 nsamples: Int,
                 forbiddenTokens: Set[Token]
               ): StochasticState = {
    Contract.require(nsamples > 0)
    val solutionPointOpt = StochasticLearner.simplifyIfSolutionPoint(problem, evaluator, scorer, currState.cOut)
    solutionPointOpt.getOrElse {
      val StochasticState(currPos, _, _) = currState
      val sconfig = StochasticLearner.samplePaths(dgraph, problem.outputRels, currPos, nsamples)

      val sampleLoss = scorer.loss(dgraph, problem.outputRels, sconfig, problem.idb)
      val gradientLoss: TokenVec = scorer.gradientLoss(dgraph, problem.outputRels, currPos, sconfig, problem.idb)
      val delta: TokenVec = gradientLoss.unit * sampleLoss / gradientLoss.abs
      val nextPos = (currPos - delta).clip(0.0, 1.0).clip(0.01, 0.99, currPos)
      val newPos = TokenVec(problem.pos.keySet, token => {
        if (forbiddenTokens.contains(token)) 0.0
        else nextPos(token).v
      })
      StochasticState(problem, evaluator, scorer, newPos, currState.cOut)
    }
  }

}