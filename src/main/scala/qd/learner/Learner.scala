package qd
package learner

import qd.evaluator.Evaluator
import qd.instance.Config
import qd.problem.Problem
import qd.util.Contract

object Learner {

  case class State(pos: TokenVec, cOut: Config[FValue], loss: Double)

  def learn(problem: Problem, evaluator: Evaluator, scorer: Scorer, tgtLoss: Double, maxIters: Int): State = {
    val trace = descend(problem, evaluator, scorer, tgtLoss, maxIters)
    val bestState = trace.minBy(_.loss)
    val highTrace = keepHighestTokens(problem, evaluator, scorer, bestState)
    val usefulTrace = highTrace.map(state => keepUsefulRules(problem, evaluator, scorer, state))
    val reinterpretTrace = usefulTrace.map(state => reinterpret(problem, evaluator, scorer, state))
    reinterpretTrace.minBy(_.loss)
  }

  def descend(problem: Problem, evaluator: Evaluator, scorer: Scorer, tgtLoss: Double, maxIters: Int): Seq[State] = {
    val random = new scala.util.Random()

    var currPos = TokenVec(problem.allTokens.map(token => token -> (0.25 + random.nextDouble() / 0.5)).toMap)
    var currOut = evaluator(problem.rules, currPos, problem.edb)
    var currLoss = scorer.loss(currOut, problem.idb, problem.outputRels)
    var currState = State(currPos, currOut, currLoss)
    var ans = Seq(currState)

    var grad = scorer.gradientLoss(currPos, currOut, problem.idb, problem.outputRels)
    var step = currPos

    val startTime = System.nanoTime()
    while (ans.size < maxIters && currLoss >= tgtLoss && grad.abs > 0 && step.abs > 0.0) {
      val oldPos = currPos

      val delta = grad.unit * currLoss / grad.abs
      currPos = (currPos - delta).clip(0.0, 1.0).clip(0.01, 0.99, currPos)
      currOut = evaluator(problem.rules, currPos, problem.edb)
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

  def keepHighestTokens(problem: Problem, evaluator: Evaluator, scorer: Scorer, state: State): Seq[State] = {
    val sortedTokens = state.pos.toSeq.sortBy(-_._2).map(_._1)

    scribe.info("Keeping highest tokens")
    for (k <- Range(1, sortedTokens.size + 1))
    yield {
      val highTokens = sortedTokens.take(k).toSet

      val pos = TokenVec(problem.allTokens, t => if (highTokens.contains(t)) state.pos.map(t) else 0.0)
      val cOut = evaluator(problem.rules, pos, problem.edb)
      val loss = scorer.loss(cOut, problem.idb, problem.outputRels)

      scribe.info(s"$k ${highTokens.size} $loss")
      State(pos, cOut, loss)
    }
  }

  def keepUsefulRules(problem: Problem, evaluator: Evaluator, scorer: Scorer, state: State): State = {
    scribe.info("Preserving useful tokens")
    val usefulTokens = for (rel <- problem.outputRels;
                            (_, v) <- state.cOut(rel).support;
                            token <- v.l.tokenSet)
                       yield token

    val pos = TokenVec(problem.allTokens, t => if (usefulTokens.contains(t)) state.pos.map(t) else 0.0)
    val cOut = evaluator(problem.rules, pos, problem.edb)
    val loss = scorer.loss(cOut, problem.idb, problem.outputRels)
    Contract.assert(cOut == state.cOut && loss == state.loss)

    scribe.info(s"P $loss ${usefulTokens.size}")
    State(pos, cOut, loss)
  }

  def reinterpret(problem: Problem, evaluator: Evaluator, scorer: Scorer, state: State): State = {
    scribe.info("Reinterpreting program")

    val pos = TokenVec(problem.allTokens, t => if (state.pos.map(t) > 0.0) 1.0 else 0.0)
    val cOut = evaluator(problem.rules, pos, problem.edb)
    val loss = scorer.loss(cOut, problem.idb, problem.outputRels)

    scribe.info(s"R $loss")
    State(pos, cOut, loss)
  }

}
