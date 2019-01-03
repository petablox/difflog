package qd
package evaluator

import qd.evaluator.TrieEvaluator.RuleTrie
import qd.instance.{AssignmentTrie, Config}

object TrieJoinEvaluator extends Evaluator {

  override val toString: String = "TrieJoinEvaluator"

  override def apply[T <: Value[T]](rules: Set[Rule], pos: Token => T, edb: Config[T])
                                   (implicit vs: Semiring[T]): Config[T] = {
    var variableIndex = Map[Variable, Int]()
    for (v <- rules.flatMap(_.variables)) { variableIndex = variableIndex + (v -> variableIndex.size) }
    implicit val ordering: Ordering[Variable] = (x: Variable, y: Variable) => variableIndex(x).compare(variableIndex(y))

    val trie = RuleTrie(rules)
    val allLiterals = rules.flatMap(_.body).groupBy(_.relation)
    val asgns = rules.flatMap(_.body)
                     .map(literal => (literal, AssignmentTrie(edb(literal.relation).support, literal)))
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
                                   assignments: Map[Literal, AssignmentTrie[T]],
                                   changed: Boolean
                                 )(implicit val ordering: Ordering[Variable], implicit val vs: Semiring[T]) {

    def addTuples(relation: Relation, newTuples: Seq[(DTuple, T)]): State[T] = {
      val oldInstance = config(relation)
      val newInstance = newTuples.foldLeft(oldInstance)(_ + _)
      val newConfig = config + (relation -> newInstance)

      var newAssignments = assignments
      for (literal <- allLiterals(relation)) {
        val oldLas = newAssignments(literal)
        val deltaLas = AssignmentTrie(newTuples, literal)
        assert(deltaLas.signature == oldLas.signature)
        val newLas = AssignmentTrie(oldLas.signature, oldLas.instance ++ deltaLas.instance)
        newAssignments = newAssignments + (literal -> newLas)
      }

      val newChanged = changed || newTuples.exists { case (tuple, value) => value > oldInstance(tuple) }
      State(trie, pos, allLiterals, newConfig, newAssignments, newChanged)
    }

    def nextEpoch: State[T] = if (!changed) this else State(trie, pos, allLiterals, config, assignments, changed = false)

    override def toString: String = {
      val n = System.lineSeparator()
      val buffer = new StringBuilder()
      buffer.append(s"===$n")
      for ((relation, literals) <- allLiterals) {
        buffer.append(s"---$n")
        buffer.append(s"Relation $relation$n")
        buffer.append(s"  Instance: ${config(relation)}$n")
        for (literal <- literals) {
          buffer.append(s"  Literal $literal: ${assignments(literal)}$n")
        }
      }
      buffer.toString()
    }

  }

  def immediateConsequence[T <: Value[T]](state: State[T]): State[T] = {
    implicit val ordering: Ordering[Variable] = state.ordering
    implicit val vs: Semiring[T] = state.vs
    immediateConsequence(state, state.trie, AssignmentTrie())
  }

  // Applies a RuleTrie to a configuration
  def immediateConsequence[T <: Value[T]](
                                           state: State[T],
                                           trie: RuleTrie,
                                           assignments: AssignmentTrie[T]
                                         ): State[T] = {
    implicit val ordering: Ordering[Variable] = state.ordering
    implicit val vs: Semiring[T] = state.vs

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
      val ax3 = AssignmentTrie.join(assignments, state.assignments(literal))
      nextState = immediateConsequence(nextState, subTrie, ax3)
    }

    // Step 2: Process leaves
    for (rule <- trie.leaves) {
      val ax3 = ax2 * Value(rule.lineage, state.pos)
      val newTuples = ax3.support.map(_.toTuple(rule.head))
      nextState = nextState.addTuples(rule.head.relation, newTuples)
    }

    nextState
  }

}
