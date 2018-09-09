package qd
package evaluator

import qd.problem.Problem.NormMode

abstract class Evaluator {
  def apply[T <: Value[T]](rules: Set[Rule[T]], normMode: NormMode, edb: Config[T])
                          (implicit vs: Semiring[T]): Config[T] = ???
  def apply[T <: Value[T]](rules: Set[Rule[T]], edb: Config[T])(implicit vs: Semiring[T]): Config[T]
}
