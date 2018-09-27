package qd
package evaluator

import scala.collection.parallel.{ParMap, ParSeq}
import TrieEvaluator.RuleTrie

object TrieSemiEvaluator extends Evaluator {

  override def apply[T <: Value[T]](rules: Set[Rule[T]], edb: Config[T])(implicit vs: Semiring[T]): Config[T] = {
    // val trie = RuleTrie(rules.map(_.normalized))
    val oldMaxValency = rules.map(_.valency).max
    val newRules = rules.map(_.normalized)
    val newMaxValency = newRules.map(_.valency).max
    val trie = RuleTrie(newRules)
    var state = State(trie, edb, Config(), edb)
    println(s"!!! $oldMaxValency $newMaxValency ${trie.numLiterals} ${rules.toSeq.map(_.body.size).sum}")
    while (state.changed) { state = immediateConsequence(state.nextEpoch) }
    state.config
  }

  case class State[T <: Value[T]](trie: RuleTrie[T], config: Config[T], deltaCurr: Config[T], deltaNext: Config[T])
                                 (implicit val vs: Semiring[T]) {

    def changed: Boolean = deltaCurr.nonEmptySupport || deltaNext.nonEmptySupport

    def addTuples(relation: Relation, newTuples: ParMap[DTuple, T]): State[T] = {
      val oldInstance = config(relation)
      val ntp = newTuples.filter { case (tuple, value) => value > oldInstance(tuple) }

      val newInstance = ntp.foldLeft(oldInstance)(_ + _)
      val newConfig = config + (relation -> newInstance)

      val oldDCInstance = deltaCurr(relation)
      val newDCInstance = ntp.foldLeft(oldDCInstance)(_ + _)
      val newDeltaCurr = deltaCurr + (relation -> newDCInstance)

      val oldDNInstance = deltaNext(relation)
      val newDNInstance = ntp.foldLeft(oldDNInstance)(_ + _)
      val newDeltaNext = deltaNext + (relation -> newDNInstance)

      State(trie, newConfig, newDeltaCurr, newDeltaNext)
    }

    def nextEpoch: State[T] = if (!changed) this else State(trie, config, deltaNext, Config())

  }

  def immediateConsequence[T <: Value[T]](state: State[T]): State[T] = {
    implicit val vs: Semiring[T] = state.vs
    immediateConsequence(state, state.trie, ParSeq(Assignment.Empty()), deltaDone = false)
  }

  // Applies a RuleTrie to a configuration
  def immediateConsequence[T <: Value[T]](
                                           state: State[T],
                                           trie: RuleTrie[T],
                                           assignments: ParSeq[Assignment[T]],
                                           deltaDone: Boolean
                                         ): State[T] = {
    // Step 0: Collapse assignments.
    // Elided because early experiments showed no reductions in number of assignments, and
    // caused a slow-down while computing immediate consequences
    /* val ax0 = assignments
    val ax1 = ax0.map(_.project(trie.variables))
    val ax2 = ax1.groupBy(_.map)
                 .mapValues(_.map(_.score).foldLeft(vs.Zero)(_ + _))
                 .toSeq.map(mv => Assignment(mv._1, mv._2)) */
    val ax2 = assignments

    var nextState = state

    // Step 1: Process sub-tries
    for ((literal, subTrie) <- trie.map) {
      // Extend with full configuration only if there is the possibility to skip work later
      if (subTrie.numLiterals > 0 || deltaDone) {
        val ax3 = extendAssignments(literal, nextState.config, ax2)
        nextState = immediateConsequence(nextState, subTrie, ax3, deltaDone)
      }

      // Skip work only if never skipped work in the past
      if (!deltaDone) {
        val ax4 = extendAssignments(literal, nextState.deltaCurr, ax2)
        nextState = immediateConsequence(nextState, subTrie, ax4, deltaDone = true)
      }
    }

    // Step 2: Process leaves
    if (deltaDone) {
      for (rule <- trie.leaves) {
        val newTuples = ax2.map(_ * rule.coeff).map(_.toTuple(rule.head)).toMap
        nextState = nextState.addTuples(rule.head.relation, newTuples)
      }
    }

    nextState
  }

  def extendAssignments[T <: Value[T]](
                                        literal: Literal,
                                        config: Config[T],
                                        assignments: ParSeq[Assignment[T]]
                                      ): ParSeq[Assignment[T]] = {
    for (assignment <- assignments;
         f = assignment.toFilter(literal);
         (tuple, score) <- config(literal.relation).filter(f);
         newAssignment <- extendAssignment(literal, tuple, assignment))
      yield newAssignment * score
  }

  def extendAssignment[T <: Value[T]](
                                       literal: Literal,
                                       tuple: DTuple,
                                       assignment: Assignment[T]
                                     ): Option[Assignment[T]] = {
    var ans = assignment
    for ((par, field) <- literal.fields.zip(tuple)) {
      par match {
        case v @ Variable(_, _) =>
          if (!ans.contains(v)) ans = ans + (v -> field)
          else if (ans(v) != field) return None
        case Constant(c, _) => if (c != field) return None
      }
    }
    Some(ans)
  }

}
