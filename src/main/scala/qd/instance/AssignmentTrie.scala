package qd
package instance

import scala.collection.SortedMap

case class AssignmentTrie[T <: Value[T]](signature: IndexedSeq[Variable], instance: Instance[T])
                                        (implicit ordering: Ordering[Variable]) {
  require(Range(0, signature.size - 1).forall(i => ordering.lt(signature(i), signature(i + 1))))
  require(signature.map(_.domain) == instance.signature)
  def *(t: T): AssignmentTrie[T] = AssignmentTrie(signature, instance * t)
}

object AssignmentTrie {

  def apply[T <: Value[T]](instance: Instance[T], bodyLit: Literal)
                          (implicit ordering: Ordering[Variable], vs: Semiring[T]): AssignmentTrie[T] = {
    require(instance.signature == bodyLit.relation.signature)

    val signature = bodyLit.variables.toVector.sorted
    val instAns = Instance[T](signature.map(_.domain))

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

  def ground[T <: Value[T]](at: AssignmentTrie[T])
                           (implicit ordering: Ordering[Variable], vs: Semiring[T]): Map[Map[Variable, Constant], T] = {
    at.instance match {
      case InstanceBase(value) => Map(Map[Variable, Constant]() -> value)
      case InstanceInd(_, _, map) =>
        for ((c, inst) <- map; (m, v) <- ground(AssignmentTrie(at.signature.tail, inst)))
        yield (m + (at.signature.head -> c)) -> v
    }
  }

  def ground[T <: Value[T]](at: AssignmentTrie[T], head: Literal)
                           (implicit ordering: Ordering[Variable], vs: Semiring[T]): Instance[T] = {
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

    def joinRec(sign1: IndexedSeq[Variable], inst1: Instance[T],
                sign2: IndexedSeq[Variable], inst2: Instance[T]): Instance[T] = {

      if (sign1.nonEmpty && sign2.nonEmpty) {

        val (head1, head2) = (sign1.head, sign2.head)
        if (ordering.lt(head1, head2)) {
          val InstanceInd(domHead, _, map1) = inst1
          val domTailAns = (sign1.tail ++ sign2).distinct.sorted.map(_.domain)
          val mapAns = for ((c1, instTl1) <- map1) yield c1 -> joinRec(sign1.tail, instTl1, sign2, inst2)
          InstanceInd(domHead, domTailAns, mapAns)
        } else if (ordering.equiv(head1, head2)) {
          val InstanceInd(domHead, domTail1, map1) = inst1
          val InstanceInd(_, domTail2, map2) = inst2
          val domTailAns = (sign1.tail ++ sign2.tail).distinct.sorted.map(_.domain)
          val mapAns = (for (c <- map1.keySet ++ map2.keySet;
                             instTl1 = map1.getOrElse(c, Instance(domTail1));
                             instTl2 = map2.getOrElse(c, Instance(domTail2));
                             instTl = joinRec(sign1.tail, instTl1, sign2.tail, instTl2))
                        yield c -> instTl).toMap
          InstanceInd(domHead, domTailAns, mapAns)
        } else {
          assert(ordering.gt(head1, head2))
          val InstanceInd(domHead, _, map2) = inst2
          val domTailAns = (sign1 ++ sign2.tail).distinct.sorted.map(_.domain)
          val mapAns = for ((c2, instTl2) <- map2) yield c2 -> joinRec(sign1, inst1, sign2.tail, instTl2)
          InstanceInd(domHead, domTailAns, mapAns)
        }

      } else if (sign1.isEmpty) {
        val InstanceBase(v1) = inst1
        inst2 * v1
      } else {
        assert(sign2.isEmpty)
        val InstanceBase(v2) = inst2
        inst1 * v2
      }

    }

    val signature = (at1.signature ++ at2.signature).sorted.distinct
    val instance = joinRec(at1.signature, at1.instance, at2.signature, at2.instance)
    AssignmentTrie(signature, instance)

  }

  def project[T <: Value[T]](at: AssignmentTrie[T], varSet: Set[Variable])
                            (implicit ordering: Ordering[Variable], vs: Semiring[T]): AssignmentTrie[T] = {

    def pi(instance: Instance[T], bv: IndexedSeq[Boolean], varVec: IndexedSeq[Variable]): Instance[T] = instance match {
      case InstanceBase(_) => instance

      case InstanceInd(domHead, _, map) if bv.head =>
        val domTailAns = varVec.tail.map(_.domain)
        val mmap = for ((c, inst) <- map) yield c -> pi(inst, bv.tail, varVec.tail)
        InstanceInd(domHead, domTailAns, mmap)

      case InstanceInd(_, _, map) if !bv.head =>
        val domAns = varVec.map(_.domain)
        map.values.foldLeft(Instance(domAns))((sum, inst) => sum ++ pi(inst, bv.tail, varVec))
    }

    val varVec = varSet.toVector.sorted
    AssignmentTrie(varVec, pi(at.instance, at.signature.map(varSet), varVec))

  }

}
