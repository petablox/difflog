package qd
package evaluator

abstract class Evaluator {
  def apply[T <: Value[T]](rules: Set[Rule[T]], edb: Config[T])(implicit vs: Semiring[T]): Config[T]
  def apply[T <: Value[T]](program: Program[T], edb: Config[T])(implicit vs: Semiring[T]): Config[T] = {
    apply(program.rules, edb)
  }
}

abstract class EvaluatorShunt[T <: Value[T]] extends (Config[T] => Config[T]) {
  val name: Any
  val program: Program[T]
  val rules: Set[Rule[T]] = program.rules
}
