package qd
package evaluator

import scala.collection.parallel.ParSeq

case class TrieEvaluator[T <: Value[T]](program: Program[T])(implicit vs: Semiring[T])
extends Evaluator[T]("Trie") {

  val fullTrie: RuleTrie[T] = RuleTrie(program.rules.map(_.normalize))

  override def apply(edb: Config[T]): Config[T] = {
    var (config, changed) = (edb, true)
    while (changed) {
      val cd = immediateConsequence(config)
      config = cd._1
      changed = cd._2
    }
    config
  }

  def immediateConsequence(config: Config[T]): (Config[T], Boolean) = {
    immediateConsequence(ParSeq(Assignment.Empty()), fullTrie, config)
  }

  // Applies a trie to a configuration
  def immediateConsequence(
                            assignments: ParSeq[Assignment[T]],
                            trie: RuleTrie[T],
                            config: Config[T]
                          ): (Config[T], Boolean) = {
    var (newConfig, changed) = (config, false)

    // Step 0: Collapse assignments
    /* val ax0 = assignments
    val ax1 = ax0.map(_.project(trie.variables))
    val ax2 = ax1.groupBy(_.map)
                 .mapValues(_.map(_.score).foldLeft(vs.Zero)(_ + _))
                 .toSeq.map(mv => Assignment(mv._1, mv._2)) */
    val ax2 = assignments

    // Step 1: Process sub-tries
    for ((literal, subTrie) <- trie.map) {
      val ax3 = extend(literal, newConfig, ax2)
      val subCall = immediateConsequence(ax3, subTrie, newConfig)
      newConfig = subCall._1
      changed = changed || subCall._2
    }

    // Step 2: Process leaves
    for (rule <- trie.leaves) {
      val newTuples = ax2.map(_ * rule.coeff).map(_.toTuple(rule.head)).toMap
      val relation = rule.head.relation
      val oldInstance = newConfig(relation)
      val newInstance = newTuples.foldLeft(oldInstance)(_ + _)
      newConfig = config + (relation -> newInstance)
      changed = changed || newTuples.exists { case (tuple, value) => value > oldInstance(tuple) }
    }

    (newConfig, changed)
  }

  // Applies a rule to a configuration
  def immediateConsequence(rule: Rule[T], config: Config[T]): (Config[T], Boolean) = {
    var bodyVals = ParSeq(Assignment.Empty)
    var remainingLits = rule.body
    for (literal <- rule.body) {
      bodyVals = extend(literal, config, bodyVals)

      remainingLits = remainingLits - literal
      val relevantVars = remainingLits.map(_.variables).foldLeft(rule.head.variables)(_ ++ _)
      bodyVals = bodyVals.map(_.project(relevantVars))
      bodyVals = bodyVals.groupBy(_.map)
                         .mapValues(_.map(_.score).foldLeft(vs.Zero: T)(_ + _))
                         .toSeq.map(mv => Assignment(mv._1, mv._2))
    }
    val newTuples = bodyVals.map(_ * rule.coeff).map(_.toTuple(rule.head)).toMap

    val relation = rule.head.relation
    val oldInstance = config(relation)
    val newInstance = newTuples.foldLeft(oldInstance)(_ + _)
    val newConfig = config + (relation -> newInstance)

    val changed = newTuples.exists { case (tuple, value) => value > oldInstance(tuple) }

    (newConfig, changed)
  }

  def extend(literal: Literal, config: Config[T], bodyVals: ParSeq[Assignment[T]]): ParSeq[Assignment[T]] = {
    for (valuation <- bodyVals;
         f = valuation.toFilter(literal);
         (tuple, score) <- config(literal.relation).filter(f);
         newValuation <- extend(literal, tuple, valuation))
      yield newValuation * score
  }

  def extend(literal: Literal, tuple: DTuple, valuation: Assignment[T]): Option[Assignment[T]] = {
    var ans = valuation
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
