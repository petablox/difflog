package qd
package instance

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Assignments

case class Assignment[T <: Value[T]](map: Map[Variable, Constant], score: T) extends (Variable => Constant) {
  require(map.forall { case (key, value) => key.domain == value.domain })

  override def apply(key: Variable): Constant = map(key)
  def get(key: Variable): Option[Constant] = map.get(key)
  def contains(key: Variable): Boolean = map.contains(key)

  def +(kv: (Variable, Constant)): Assignment[T] = {
    val (key, _) = kv
    require(!map.contains(key))
    Assignment(map + kv, score)
  }
  def *(coeff: T): Assignment[T] = Assignment(map, score * coeff)

  def project(rvs: Set[Variable]): Assignment[T] = Assignment(map.filterKeys(rvs), score)

  def toTuple(lit: Literal): (DTuple, T) = {
    val cs = lit.fields.map {
      case c @ Constant(_, _) => c
      case v @ Variable(_, _) => this(v)
    }
    (DTuple(cs), score)
  }

  def toFilter(literal: Literal): IndexedSeq[Option[Constant]] = literal.fields.map {
    case v @ Variable(_, _) => map.get(v)
    case c @ Constant(_, _) => Some(c)
  }
}

object Assignment {
  def Empty[T <: Value[T]]()(implicit vs: Semiring[T]): Assignment[T] = Assignment(Map(), vs.One)
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Instances

sealed abstract class Instance[T <: Value[T]](val signature: IndexedSeq[Domain])(implicit vs: Semiring[T])
extends (DTuple => T) {
  val arity: Int = signature.length

  lazy val support: Set[(DTuple, T)] = this match {
    case InstanceBase(value) => if (vs.nonZero(value)) Set((DTuple(Vector()), value)) else Set()
    case InstanceInd(_, _, map) => for ((constant, mapA) <- map.view.toSet;
                                        (tuple, value) <- mapA.support)
                                   yield (constant +: tuple) -> value
  }

  lazy val nonEmpty: Boolean = support.nonEmpty

  override def apply(tuple: DTuple): T = {
    require(tuple.arity == this.arity)
    this match {
      case InstanceBase(value) => value
      case InstanceInd(domHead, _, map) =>
        val chead = tuple.head
        require(chead.domain == domHead)
        map.get(chead).map(_(tuple.tail)).getOrElse(vs.Zero)
    }
  }

  def filter(f: IndexedSeq[Option[Constant]]): Set[(DTuple, T)] = {
    require(f.length == this.arity)
    this match {
      case InstanceBase(value) => if (vs.nonZero(value)) Set((DTuple(Vector()), value)) else Set()
      case InstanceInd(_, _, map) =>
        f.head match {
          case Some(fh) =>
            val mfh = map.get(fh)
            if (mfh.nonEmpty) mfh.get.filter(f.tail).map { case (tuple, value) => (fh +: tuple, value) }
            else Set()
          case None =>
            for ((constant, ind) <- map.toSet;
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
      require(domH1 == domH2)
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

  def ++(tvs: Map[DTuple, T]): Instance[T] = tvs.foldLeft(this)(_ + _)
  def +(tv: (DTuple, T)): Instance[T] = {
    val (t, v) = tv
    require(t.arity == this.arity)
    this match {
      case InstanceBase(value) => InstanceBase(value + v)
      case InstanceInd(domHead, domTail, map) =>
        val chead = t.head
        require(chead.domain == domHead)
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

case class InstanceBase[T <: Value[T]](value: T)(implicit vs: Semiring[T]) extends Instance[T](Vector())

case class InstanceInd[T <: Value[T]](domHead: Domain, domTail: IndexedSeq[Domain], map: Map[Constant, Instance[T]])
                                     (implicit vs: Semiring[T])
extends Instance[T](domHead +: domTail) {
  require(map.forall { case (constant, _) => constant.domain == domHead })
  require(map.forall { case (_, instance) => instance.signature == domTail })
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Configurations

case class Config[T <: Value[T]](map: Map[Relation, Instance[T]])(implicit vs: Semiring[T])
extends (Relation => Instance[T]) {
  require(map.forall { case (relation, instance) => relation.signature == instance.signature })

  override def apply(relation: Relation): Instance[T] = map.getOrElse(relation, Instance(relation))
  def get(relation: Relation): Option[Instance[T]] = Some(this(relation))
  def +(ri: (Relation, Instance[T])): Config[T] = {
    val (relation, instance) = ri
    val newInstance = this(relation) ++ instance
    Config(map + (relation -> newInstance))
  }

  def add(relation: Relation, tuple: DTuple, value: T): Config[T] = {
    val newInstance = this(relation) + (tuple -> value)
    Config(map + (relation -> newInstance))
  }

  def nonEmptySupport: Boolean = map.values.exists(_.nonEmpty)
}

object Config {
  def apply[T <: Value[T]](firstPair: (Relation, Instance[T]), remainingPairs: (Relation, Instance[T])*)
                          (implicit vs: Semiring[T]): Config[T] = {
    Config((firstPair +: remainingPairs).toMap)
  }
  def apply[T <: Value[T]]()(implicit vs: Semiring[T]): Config[T] = Config[T](Map[Relation, Instance[T]]())
}
