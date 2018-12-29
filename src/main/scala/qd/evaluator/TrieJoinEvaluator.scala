package qd
package evaluator

import qd.evaluator.TrieEvaluator.RuleTrie
import qd.instance.{Assignment, AssignmentTrie, Config}

import scala.collection.parallel.{ParMap, ParSeq}

object TrieJoinEvaluator extends Evaluator {

  override val toString: String = "TrieEvaluator"

  override def apply[T <: Value[T]](rules: Set[Rule], pos: Token => T, edb: Config[T])
                                   (implicit vs: Semiring[T]): Config[T] = {
    var variableIndex = Map[Variable, Int]()
    for (v <- rules.flatMap(_.variables)) { variableIndex = variableIndex + (v -> variableIndex.size) }
    implicit val ordering: Ordering[Variable] = (x: Variable, y: Variable) => variableIndex(x).compare(variableIndex(y))

    val trie = RuleTrie(rules)
    val allLiterals = rules.flatMap(_.body).groupBy(_.relation)
    val asgns = rules.flatMap(_.body)
                     .map(literal => (literal, AssignmentTrie.fromInstance(edb(literal.relation), literal)))
                     .toMap

    var state = State(trie, pos, allLiterals, edb, asgns, changed = true)
    while (state.changed) { state = immediateConsequence(state.nextEpoch) }
    state.config
  }

  case class State[T <: Value[T]](
                                   trie: RuleTrie,
                                   pos: Token => T,
                                   allLiterals: Map[Relation, Set[Literal]],
                                   config: Config[T],
                                   asgns: Map[Literal, AssignmentTrie[T]],
                                   changed: Boolean
                                 )(implicit ordering: Ordering[Variable], implicit val vs: Semiring[T]) {

    def addTuples(relation: Relation, newTuples: ParMap[DTuple, T]): State[T] = {
      val oldInstance = config(relation)
      val newInstance = newTuples.foldLeft(oldInstance)(_ + _)
      val newConfig = config + (relation -> newInstance)
      val newChanged = changed || newTuples.exists { case (tuple, value) => value > oldInstance(tuple) }
      State(trie, pos, allLiterals, newConfig, ???, newChanged)
    }

    def nextEpoch: State[T] = if (!changed) this else State(trie, pos, allLiterals, config, asgns, changed = false)

  }

  def immediateConsequence[T <: Value[T]](state: State[T]): State[T] = {
    implicit val vs: Semiring[T] = state.vs
    immediateConsequence(state, state.trie, ParSeq(Assignment.Empty()))
  }

  // Applies a RuleTrie to a configuration
  def immediateConsequence[T <: Value[T]](
                                           state: State[T],
                                           trie: RuleTrie,
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
      implicit val vs: Semiring[T] = state.vs
      val newTuples = ax2.map(_ * Value(rule.lineage, state.pos)).map(_.toTuple(rule.head)).toMap
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
