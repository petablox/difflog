package qd
package instance

case class AssignmentTrie[T <: Value[T]](signature: IndexedSeq[Variable], instance: Instance[T]) {
  require(signature.map(_.domain) == instance.signature)

  def ground(head: Literal): Instance[T] = ???
  def *(t: T): AssignmentTrie[T] = AssignmentTrie(signature, instance * t)
}

object AssignmentTrie {
  def apply[T <: Value[T]](bodyLit: Literal, ordering: Ordering[Variable], instance: Instance[T]): AssignmentTrie[T] = {
    ???
  }

  def join[T <: Value[T]](at1: AssignmentTrie[T], at2: AssignmentTrie[T], ordering: Ordering[Variable]): AssignmentTrie[T] = ???
}
