package qd
package instance

import qd.util.Contract

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Instances

sealed abstract class Instance[T <: Value[T]](implicit vs: Semiring[T])
  extends (DTuple => T) {

  lazy val signature: IndexedSeq[Domain] = this match {
    case InstanceBase(_) => Vector()
    case InstanceInd(domHead, domTail, _) => domHead +: domTail
  }

  lazy val arity: Int = signature.length

  lazy val nonEmpty: Boolean = this match {
    case InstanceBase(value) => vs.nonZero(value)
    case InstanceInd(_, _, map) => map.values.exists(_.nonEmpty)
  }

  lazy val isEmpty: Boolean = !this.nonEmpty

  lazy val rawSupport: Vector[(Vector[Constant], T)] = this match {
    case InstanceBase(value) => if (vs.nonZero(value)) Vector((Vector(), value)) else Vector()
    case InstanceInd(_, _, map) => for ((constant, mapA) <- map.toVector;
                                        (tuple, value) <- mapA.rawSupport)
                                   yield (constant +: tuple) -> value
  }

  lazy val support: Vector[(DTuple, T)] = rawSupport.map { case (cs, v) => (DTuple(cs), v) }

  override def apply(tuple: DTuple): T = {
    Contract.require(tuple.arity == this.arity)
    this match {
      case InstanceBase(value) => value
      case InstanceInd(domHead, _, map) =>
        val chead = tuple.head
        Contract.require(chead.domain == domHead)
        map.get(chead).map(_(tuple.tail)).getOrElse(vs.Zero)
    }
  }

  def filter(f: IndexedSeq[Option[Constant]]): Vector[(DTuple, T)] = {
    Contract.require(f.length == this.arity)
    this match {
      case InstanceBase(value) => if (vs.nonZero(value)) Vector((DTuple(Vector()), value)) else Vector()
      case InstanceInd(_, _, map) =>
        f.head match {
          case Some(fh) =>
            val mfh = map.get(fh)
            if (mfh.nonEmpty) mfh.get.filter(f.tail).map { case (tuple, value) => (fh +: tuple, value) }
            else Vector()
          case None =>
            for ((constant, ind) <- map.toVector;
                 (tuple, value) <- ind.filter(f.tail))
            yield (constant +: tuple, value)
        }
    }
  }

  def *(t: T): Instance[T] = this match {
    case InstanceBase(value) => InstanceBase(value * t)
    case InstanceInd(domHead, domTail, map) =>
      InstanceInd(domHead, domTail, map.map { case (c, inst) => c -> inst * t })
  }

  def ++(that: Instance[T]): Instance[T] = (this, that) match {
    case (InstanceBase(vthis), InstanceBase(vthat)) => InstanceBase(vthis + vthat)
    case (InstanceInd(domH1, domT1, map1), InstanceInd(domH2, _, map2)) =>
      Contract.require(domH1 == domH2)
      val nm1 = for ((chead, v1) <- map1)
                yield {
                  val ov2 = map2.get(chead)
                  val v12 = if (ov2.nonEmpty) v1 ++ ov2.get else v1
                  chead -> v12
                }
      val nm2 = for ((chead, v2) <- map2 if !nm1.contains(chead))
                yield chead -> v2
      val newMap = nm1 ++ nm2
      InstanceInd(domH1, domT1, newMap)
    case (_, _) => throw new IllegalArgumentException
  }

  def ++(tvs: IndexedSeq[(DTuple, T)]): Instance[T] = {
    if (tvs.isEmpty) {
      this
    } else if (tvs.head._1.isEmpty) {
      val InstanceBase(thisv) = this
      Contract.require(tvs.forall(_._1.isEmpty))
      val thatv = tvs.foldLeft(thisv) { case (v, (_, vp)) => v + vp }
      InstanceBase(thatv)
    } else {
      val InstanceInd(domHead, domTail, map) = this
      var ans = map
      for ((cHead, subTvs) <- tvs.groupBy(_._1.head)) {
        Contract.require(cHead.domain == domHead)
        val subTvsTail = subTvs.map { case (t, v) => (t.tail, v) }
        val newInstance = ans.getOrElse(cHead, Instance(domTail)) ++ subTvsTail
        ans = ans + (cHead -> newInstance)
      }
      InstanceInd(domHead, domTail, ans)
    }
  }

  def +(tv: (DTuple, T)): Instance[T] = {
    val (t, v) = tv
    Contract.require(t.arity == this.arity)
    this match {
      case InstanceBase(value) => InstanceBase(value + v)
      case InstanceInd(domHead, domTail, map) =>
        val chead = t.head
        Contract.require(chead.domain == domHead)
        val mapA = map.getOrElse(chead, Instance(domTail))
        val newMapA = mapA + (t.tail -> v)
        InstanceInd(domHead, domTail, map + (chead -> newMapA))
    }
  }

  override def toString: String = {
    val elems = support.map { case (tuple, value) => s"$tuple: $value" }
    val elemStr = elems.mkString(", ")
    s"Instance($signature, $elemStr)"
  }
}

object Instance {
  def apply[T <: Value[T]](signature: IndexedSeq[Domain])(implicit vs: Semiring[T]): Instance[T] = {
    if (signature.isEmpty) InstanceBase(vs.Zero)
    else InstanceInd(signature.head, signature.tail, Map())
  }
  def apply[T <: Value[T]](relation: Relation)(implicit vs: Semiring[T]): Instance[T] = Instance(relation.signature)
}

private[instance] case class InstanceBase[T <: Value[T]](value: T)(implicit vs: Semiring[T]) extends Instance[T]

private[instance] case class InstanceInd[T <: Value[T]](
                                                         domHead: Domain, domTail: IndexedSeq[Domain],
                                                         map: Map[Constant, Instance[T]]
                                                       )(implicit vs: Semiring[T]) extends Instance[T] {
  Contract.deepRequire(map.forall { case (constant, _) => constant.domain == domHead })
  Contract.deepRequire(map.forall { case (_, instance) => instance.signature == domTail })
}
