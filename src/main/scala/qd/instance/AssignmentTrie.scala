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

  def fromInstance[T <: Value[T]](instance: Instance[T], literal: Literal)
                                 (implicit ordering: Ordering[Variable], vs: Semiring[T]): AssignmentTrie[T] = {
    require(instance.signature == literal.relation.signature)

    def _fromInstance(
                       subInstance: Instance[T],
                       fields: IndexedSeq[Parameter],
                       context: SortedMap[Variable, Constant]
                     ): Seq[(SortedMap[Variable, Constant], T)] = {
      subInstance match {
        case InstanceBase(value) =>
          assert(fields.isEmpty)
          Seq((context, value))
        case InstanceInd(instHeadDom, _, map) =>
          val fieldHead = fields.head
          assert(fieldHead.domain == instHeadDom)

          fieldHead match {
            case cfh @ Constant(_, _) if map.contains(cfh) => _fromInstance(map(cfh), fields.tail, context)
            case Constant(_, _) => Seq()

            case vfh @ Variable(_, _) if !context.contains(vfh) =>
              for ((cfh, subSubInstance) <- map.toSeq;
                   mv <- _fromInstance(subSubInstance, fields.tail, context + (vfh -> cfh)))
              yield mv
            case vfh @ Variable(_, _) if map.contains(context(vfh)) =>
              _fromInstance(map(context(vfh)), fields.tail, context)
            case Variable(_, _) => Seq()
          }
      }
    }

    val signature = literal.variables.toVector.sorted
    val tvs = _fromInstance(instance, literal.fields, SortedMap()).map({ case (m, v) => (DTuple(m.values.toVector), v)})
    val trie = tvs.foldLeft(Instance(signature.map(_.domain)))(_ + _)
    AssignmentTrie(signature, trie)
  }

  def ground[T <: Value[T]](at: AssignmentTrie[T]): Map[Map[Variable, Constant], T] = {
    def _ground(signature: IndexedSeq[Variable], instance: Instance[T]): Map[Map[Variable, Constant], T] = {
      instance match {
        case InstanceBase(value) =>
          assert(signature.isEmpty)
          Map(Map[Variable, Constant]() -> value)
        case InstanceInd(_, _, map) =>
          for ((c, instTl) <- map; (m, v) <- _ground(signature.tail, instTl))
          yield (m + (signature.head -> c)) -> v
      }
    }
    _ground(at.signature, at.instance)
  }

  def toInstance[T <: Value[T]](at: AssignmentTrie[T], head: Literal)
                               (implicit vs: Semiring[T]): Instance[T] = {
    var ans = Instance(head.relation.signature)
    for ((m, v) <- ground(at).seq) {
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

    def _join(sign1: IndexedSeq[Variable], inst1: Instance[T],
              sign2: IndexedSeq[Variable], inst2: Instance[T]): Instance[T] = {

      if (sign1.nonEmpty && sign2.nonEmpty) {

        val (head1, head2) = (sign1.head, sign2.head)
        if (ordering.lt(head1, head2)) {
          val InstanceInd(domHead, _, map1) = inst1
          val domTailAns = (sign1.tail ++ sign2).distinct.sorted.map(_.domain)
          val mapAns = for ((c1, instTl1) <- map1) yield c1 -> _join(sign1.tail, instTl1, sign2, inst2)
          InstanceInd(domHead, domTailAns, mapAns)
        } else if (ordering.equiv(head1, head2)) {
          val InstanceInd(domHead, domTail1, map1) = inst1
          val InstanceInd(_, domTail2, map2) = inst2
          val domTailAns = (sign1.tail ++ sign2.tail).distinct.sorted.map(_.domain)
          val mapAns = (for (c <- map1.keySet ++ map2.keySet;
                             instTl1 = map1.getOrElse(c, Instance(domTail1));
                             instTl2 = map2.getOrElse(c, Instance(domTail2));
                             instTl = _join(sign1.tail, instTl1, sign2.tail, instTl2))
                        yield c -> instTl).toMap
          InstanceInd(domHead, domTailAns, mapAns)
        } else {
          assert(ordering.gt(head1, head2))
          val InstanceInd(domHead, _, map2) = inst2
          val domTailAns = (sign1 ++ sign2.tail).distinct.sorted.map(_.domain)
          val mapAns = for ((c2, instTl2) <- map2) yield c2 -> _join(sign1, inst1, sign2.tail, instTl2)
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
    val instance = _join(at1.signature, at1.instance, at2.signature, at2.instance)
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
