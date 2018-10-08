package qd
package evaluator

import qd.instance.Config

abstract class Evaluator {
  def apply[T <: Value[T]](rules: Set[Rule[T]], edb: Config[T])(implicit vs: Semiring[T]): Config[T]
}

object Evaluator {
  val STD_EVALUATORS = Map("Naive" -> NaiveEvaluator,
                           "Seminaive" -> SeminaiveEvaluator,
                           "Trie" -> TrieEvaluator,
                           "TrieSemi" -> TrieSemiEvaluator)
}
