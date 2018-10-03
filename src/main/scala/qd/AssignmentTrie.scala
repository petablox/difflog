package qd

case class AssignmentTrie[T <: Value[T]](signature: IndexedSeq[Variable], instance: Instance[T]) {
  require(signature.map(_.domain) == instance.signature)
}
