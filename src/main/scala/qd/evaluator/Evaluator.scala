package qd
package evaluator

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Evaluators

abstract class Evaluator[T <: Value[T]](val name: Any) extends (Config[T] => Config[T]) {
  val program: Program[T]
  val rules: Set[Rule[T]] = program.rules
  val relations: Set[Relation] = program.relations
  val domains: Set[Domain] = program.domains
}
