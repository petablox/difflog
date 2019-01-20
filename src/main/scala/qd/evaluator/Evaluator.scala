package qd
package evaluator

import qd.instance.Config

abstract class Evaluator {
  override def toString: String
  def apply[T <: Value[T]](rules: Set[Rule], pos: Token => T, edb: Config[T])(implicit vs: Semiring[T]): Config[T]
}

object Evaluator {
  val STD_EVALUATORS: Map[String, Evaluator] = Map("naive" -> NaiveEvaluator,
                                                   "seminaive" -> SeminaiveEvaluator,
                                                   "trie" -> TrieEvaluator,
                                                   "trie-semi" -> TrieSemiEvaluator,
                                                   "trie-join" -> TrieJoinEvaluator)
}
