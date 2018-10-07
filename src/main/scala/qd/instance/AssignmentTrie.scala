package qd
package instance

import scala.collection.SortedMap

case class AssignmentTrie[T <: Value[T]](signature: IndexedSeq[Variable], instance: Instance[T]) {
  require(signature.distinct.size == signature.size)
  require(signature.map(_.domain) == instance.signature)
  def *(t: T): AssignmentTrie[T] = AssignmentTrie(signature, instance * t)
}

object AssignmentTrie {

  def apply[T <: Value[T]](instance: Instance[T], bodyLit: Literal)
                          (implicit ordering: Ordering[Variable], vs: Semiring[T]): AssignmentTrie[T] = {
    require(instance.signature == bodyLit.relation.signature)
    val signature = bodyLit.variables.toVector.sorted

    def buildRec(
                  inst: Instance[T],
                  fields: IndexedSeq[Parameter],
                  context: SortedMap[Variable, Constant]
                ): Instance[T] = {
      require(inst.signature == fields.map(_.domain))
      val ansVars = (fields.collect({ case v @ Variable(_, _) => v }) ++ context.keys).sorted
      val ansDoms = ansVars.map(_.domain)

      inst match {
        case InstanceBase(value) =>
          val t = DTuple(context.values.toVector)
          Instance(ansDoms) + (t -> value)

        case InstanceInd(domHead, _, map) =>
          val fieldsHead = fields.head
          val fieldsTail = fields.tail
          require(fieldsHead.domain == domHead)

          fieldsHead match {
            case cHead @ Constant(_, _) =>
              if (map.contains(cHead)) buildRec(map(cHead), fieldsTail, context) else Instance[T](ansDoms)

            case vHead @ Variable(_, _) =>
              val vBindings = if (!context.contains(vHead)) map.keySet
                              else if (map.contains(context(vHead))) Set(context(vHead))
                              else Set[Constant]()

              val tailVars = fieldsTail.collect({ case v @ Variable(_, _) => v })
              val minTailVar = tailVars.min
              var contextCurr = context.filterKeys(v => ordering.lt(v, minTailVar))
              var contextTail = context.filterKeys(v => ordering.gteq(v, minTailVar))

              var ans = Instance(ansDoms)
              for (vb <- vBindings) {
                if (ordering.lt(vHead, minTailVar)) {
                  contextCurr = contextCurr + (vHead -> vb)
                } else {
                  contextTail = contextTail + (vHead -> vb)
                }

                var recAns = buildRec(map(vb), fieldsTail, contextTail)
                for ((v, c) <- contextCurr.toSeq.reverse) {
                  assert(v.domain == c.domain)
                  recAns = InstanceInd(v.domain, recAns.signature, Map(c -> recAns))
                }
                ans = ans ++ recAns
              }
              ans
          }
      }

    }

    val trie = buildRec(instance, bodyLit.fields, SortedMap())
    AssignmentTrie(signature, trie)
  }

  def ground[T <: Value[T]](at: AssignmentTrie[T])(implicit vs: Semiring[T]): Map[Map[Variable, Constant], T] = {
    at.instance match {
      case InstanceBase(value) => Map(Map[Variable, Constant]() -> value)
      case InstanceInd(_, _, map) =>
        for ((c, inst) <- map; (m, v) <- ground(AssignmentTrie(at.signature.tail, inst)))
        yield (m + (at.signature.head -> c)) -> v
    }
  }

  def ground[T <: Value[T]](at: AssignmentTrie[T], head: Literal)(implicit vs: Semiring[T]): Instance[T] = {
    var ans = Instance(head.relation.signature)
    for ((m, v) <- ground(at)) {
      val t = head.fields.map {
        case v @ Variable(_, _) => m(v)
        case c @ Constant(_, _) => c
      }
      ans = ans + (DTuple(t) -> v)
    }
    ans
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
