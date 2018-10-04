package qd
package instance

case class AssignmentTrie[T <: Value[T]](signature: IndexedSeq[Variable], instance: Instance[T]) {
  require(signature.map(_.domain) == instance.signature)
  def *(t: T): AssignmentTrie[T] = AssignmentTrie(signature, instance * t)
}

object AssignmentTrie {

  def apply[T <: Value[T]](instance: Instance[T], bodyLit: Literal)
                          (implicit ordering: Ordering[Variable], vs: Semiring[T]): AssignmentTrie[T] = {
    require(instance.signature == bodyLit.relation.signature)

    def buildTrie(
                   inst: Instance[T],
                   fields: IndexedSeq[Parameter],
                   context: Map[Variable, Constant]
                 ): (IndexedSeq[Variable], Set[Variable], Instance[T]) = {
      inst match {
        case InstanceBase(_) => (Vector(), Set(), inst)
        case InstanceInd(domHead, _, map) =>
          val fieldsHead = fields.head
          val fieldsTail = fields.tail
          require(fieldsHead.domain == domHead)

          fieldsHead match {
            case cHead @ Constant(_, _) =>
              if (map.contains(cHead)) {
                buildTrie(map(cHead), fieldsTail, context)
              } else {
                val varVec = fieldsTail.collect { case v @ Variable(_, _) => v }
                val varSet = varVec.toSet
                val ans = Instance[T](varVec.map(_.domain))
                (varVec, varSet, ans)
              }
            case vHead @ Variable(_, _) => ???
          }
      }
    }

    val (signature, _, trie) = buildTrie(instance, bodyLit.fields, Map())
    AssignmentTrie(signature, trie)
  }

  def ground[T <: Value[T]](at: AssignmentTrie[T], head: Literal): Instance[T] = {
    ???
  }

  def join[T <: Value[T]](at1: AssignmentTrie[T], at2: AssignmentTrie[T])
                         (implicit ordering: Ordering[Variable], vs: Semiring[T]): AssignmentTrie[T] = {
    ???
  }

  def project[T <: Value[T]](at: AssignmentTrie[T], varSet: Set[Variable])
                            (implicit ordering: Ordering[Variable], vs: Semiring[T]): AssignmentTrie[T] = {
    def pi(instance: Instance[T], bv: IndexedSeq[Boolean]): Instance[T] = instance match {
      case InstanceBase(_) => instance
      case InstanceInd(domHead, domTail, map) =>
        val mmap = map.transform { case (_, inst) => pi(inst, bv.tail) }
        if (bv.head) InstanceInd(domHead, domTail, mmap)
        else mmap.values.foldLeft(Instance(domTail))(_ ++ _)
    }

    AssignmentTrie(varSet.toVector.sorted, pi(at.instance, at.signature.map(varSet)))
  }

}
