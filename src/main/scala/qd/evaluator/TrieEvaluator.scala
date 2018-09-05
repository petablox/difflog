package qd
package evaluator

import com.typesafe.scalalogging.Logger

import scala.collection.immutable.Iterable
import scala.collection.parallel.{ParMap, ParSeq}

object TrieEvaluator extends Evaluator {

  override def apply[T <: Value[T]](rules: Set[Rule[T]], edb: Config[T])(implicit vs: Semiring[T]): Config[T] = {
    val trie = RuleTrie(rules.map(_.normalize))
    var state = State(trie, edb, changed = true)
    while (state.changed) { state = immediateConsequence(state.nextEpoch) }
    state.config
  }

  case class RuleTrie[T <: Value[T]](leaves: Set[Rule[T]], map: Map[Literal, RuleTrie[T]]) extends Iterable[Rule[T]] {

    // Commented because the following check is too time-consuming
    // require(map.forall { case (literal, trie) => trie.forall(_.body.contains(literal)) })

    val numLiterals: Int = map.map({ case (_, subTrie) => 1 + subTrie.numLiterals }).sum
    val numRules: Int = leaves.size + map.values.map(_.numRules).sum
    override def iterator: Iterator[Rule[T]] = map.values.foldLeft(leaves.iterator)(_ ++ _.iterator)
    val variables: Set[Variable] = {
      val vs1 = leaves.flatMap(_.head.variables)
      val vs2 = map.flatMap { case (l, t) => l.variables ++ t.variables }
      vs1 ++ vs2
    }

    def +(rule: Rule[T]): RuleTrie[T] = {
      def add(remainingLiterals: Seq[Literal], trie: RuleTrie[T]): RuleTrie[T] = {
        if (remainingLiterals.isEmpty) RuleTrie(trie.leaves + rule, trie.map)
        else {
          val litHead = remainingLiterals.head
          val litRest = remainingLiterals.tail
          val subTrie = trie.map.getOrElse(litHead, RuleTrie())
          RuleTrie(trie.leaves, trie.map + (litHead -> add(litRest, subTrie)))
        }
      }
      add(rule.body.toSeq.sortBy(_.toString), this)
    }

  }

  object RuleTrie {
    def apply[T <: Value[T]](): RuleTrie[T] = RuleTrie(Set(), Map())
    def apply[T <: Value[T]](rules: Iterable[Rule[T]]): RuleTrie[T] = rules.foldLeft(RuleTrie[T]())(_ + _)
  }

  case class State[T <: Value[T]](trie: RuleTrie[T], config: Config[T], changed: Boolean)
                                 (implicit val vs: Semiring[T]) {

    def addTuples(relation: Relation, newTuples: ParMap[DTuple, T]): State[T] = {
      val oldInstance = config(relation)
      val newInstance = newTuples.foldLeft(oldInstance)(_ + _)
      val newConfig = config + (relation -> newInstance)
      val newChanged = changed || newTuples.exists { case (tuple, value) => value > oldInstance(tuple) }
      State(trie, newConfig, newChanged)
    }

    def nextEpoch: State[T] = if (!changed) this else State(trie, config, changed = false)

  }

  def immediateConsequence[T <: Value[T]](state: State[T]): State[T] = {
    implicit val vs: Semiring[T] = state.vs
    immediateConsequence(state, state.trie, ParSeq(Assignment.Empty()))
  }

  // Applies a RuleTrie to a configuration
  def immediateConsequence[T <: Value[T]](
                                           state: State[T],
                                           trie: RuleTrie[T],
                                           assignments: ParSeq[Assignment[T]]
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
      val ax3 = extendAssignments(literal, nextState.config, ax2)
      nextState = immediateConsequence(nextState, subTrie, ax3)
    }

    // Step 2: Process leaves
    for (rule <- trie.leaves) {
      val newTuples = ax2.map(_ * rule.coeff).map(_.toTuple(rule.head)).toMap
      nextState = nextState.addTuples(rule.head.relation, newTuples)
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
