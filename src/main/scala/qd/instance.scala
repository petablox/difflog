package qd

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Instances (of relations / predicates) and configurations

sealed abstract class Instance[T <: Value[T]](val signature: Seq[Domain])(implicit num: OneAndZero[T])extends (DTuple => T) {
  require(signature.nonEmpty)

  override def apply(tuple: DTuple): T = {
    val startTime = System.nanoTime()
    val ans = this match {
      case InstanceBase(domain, map) =>
        require(tuple.length == 1)
        val atom = tuple.head
        require(domain.contains(atom))
        map.getOrElse(atom, num.Zero)
      case InstanceInd(domHead, _, map) =>
        val atomHead = tuple.head
        require(domHead.contains(atomHead))
        map.get(atomHead).map(_(tuple.tail)).getOrElse(num.Zero)
    }
    val endTime = System.nanoTime()
    Instance.applyTime += endTime - startTime
    ans
  }

  val support: Seq[(DTuple, T)] = {
    val startTime = System.nanoTime()
    val ans = this match {
      case InstanceBase(_, map) => map.toSeq.view
        .filter({ case (_, value) => value.isNonZero })
        .map({ case (atom, value) => DTuple(atom) -> value })
      case InstanceInd(_, _, map) => for ((atom, mapA) <- map.toSeq.view;
                                          (tuple, value) <- mapA.support)
        yield (atom +: tuple) -> value
    }
    val endTime = System.nanoTime()
    Instance.supportTime += endTime - startTime
    ans
  }

  val nonEmpty: Boolean = support.nonEmpty

  def filter(f: Seq[Option[Atom]]): Seq[(DTuple, T)] = {
    val startTime = System.nanoTime()
    val ans = (this, f.head) match {
      case (InstanceBase(_, map), Some(fh)) =>
        val mfh = map.getOrElse(fh, num.Zero)
        if (mfh.isNonZero) Seq((DTuple(fh), mfh)) else Seq()
      case (InstanceBase(_, map), None) =>
        for ((atom, value) <- map.view.toSeq; if value.isNonZero) yield (DTuple(atom), value)
      case (InstanceInd(_, _, map), Some(fh)) =>
        val mfh = map.get(fh)
        if (mfh.nonEmpty) mfh.get.filter(f.tail).map { case (tuple, value) => (fh +: tuple, value) }
        else Seq()
      case (InstanceInd(_, _, map), None) =>
        for ((atom, s) <- map.mapValues(_.filter(f.tail)).toSeq; (tuple, value) <- s) yield (atom +: tuple, value)
    }
    val endTime = System.nanoTime()
    Instance.filterTime += endTime - startTime
    ans
  }

  def ++(that: Instance[T]): Instance[T] = {
    val startTime = System.nanoTime()
    val ans = (this, that) match {
      case (InstanceBase(dom1, map1), InstanceBase(dom2, map2)) =>
        require(dom1 == dom2)
        val newMap = for (atom <- map1.keySet ++ map2.keySet;
                          v1 = map1.getOrElse(atom, num.Zero);
                          v2 = map2.getOrElse(atom, num.Zero);
                          sum = v1 + v2)
          yield atom -> sum
        InstanceBase(dom1, newMap.toMap)
      case (InstanceInd(domH1, domT1, map1), InstanceInd(domH2, domT2, map2)) =>
        require(domH1 == domH2)
        val newMap = for (atom <- map1.keySet ++ map2.keySet;
                          v1 = map1.getOrElse(atom, Instance(domT1:_*));
                          v2 = map2.getOrElse(atom, Instance(domT2:_*)))
          yield atom -> (v1 ++ v2)
        InstanceInd(domH1, domT1, newMap.toMap)
      case (InstanceBase(_, _), InstanceInd(_, _, _)) => throw new IllegalArgumentException
      case (InstanceInd(_, _, _), InstanceBase(_, _)) => throw new IllegalArgumentException
    }
    val endTime = System.nanoTime()
    Instance.plusPlusInstanceTime += endTime - startTime
    ans
  }

  def ++(tvs: Map[DTuple, Value[T]]): Instance[T] = {
    val startTime = System.nanoTime()
    val ans = tvs.foldLeft(this)(_ + _)
    val endTime = System.nanoTime()
    Instance.plusPlusMapTime += endTime - startTime
    ans
  }
  def +(tv: (DTuple, Value[T])): Instance[T] = {
    val startTime = System.nanoTime()
    val ans = this.add(tv._1, tv._2)
    val endTime = System.nanoTime()
    Instance.plusTime += endTime - startTime
    ans
  }
  def add(tuple: DTuple, value: Value[T]): Instance[T] = this match {
    case InstanceBase(domain, map) =>
      require(tuple.length == 1)
      val atom = tuple.head
      require(domain.contains(atom))
      val m2 : Map[Atom, T] = map
      val oldValue = m2.getOrElse(atom, num.Zero)
      val newValue = value + oldValue
      InstanceBase(domain, map + (atom -> newValue))
    case InstanceInd(domHead, domTail, map) =>
      val atom = tuple.head
      require(domHead.contains(atom))
      val mapA = map.getOrElse(atom, Instance(domTail:_*))
      val newMapA = mapA + (tuple.tail -> value)
      InstanceInd(domHead, domTail, map + (atom -> newMapA))
  }

  override def toString: String = support.toString
}

object Instance {
  var applyTime: Long = 0
  var supportTime: Long = 0
  var filterTime: Long = 0
  var plusPlusInstanceTime: Long = 0
  var plusPlusMapTime: Long = 0
  var plusTime: Long = 0

  def apply[T <: Value[T]](signature: Domain*)(implicit num: OneAndZero[T]): Instance[T] = {
    require(signature.nonEmpty)
    if (signature.length == 1) InstanceBase(signature.head, Map())
    else InstanceInd(signature.head, signature.tail, Map())
  }
  def apply[T <: Value[T]](relation: Relation)(implicit num: OneAndZero[T]): Instance[T] = Instance(relation.signature:_*)
}

case class InstanceBase[T <: Value[T]](domain: Domain, map: Map[Atom, T])(implicit num: OneAndZero[T]) extends Instance[T](Seq(domain)) {
  require(map.keys.forall(domain.contains))
}

case class InstanceInd[T <: Value[T]](domHead: Domain, domTail: Seq[Domain], map: Map[Atom, Instance[T]])(implicit num : OneAndZero[T])
  extends Instance[T](domHead +: domTail) {
  require(map.forall { case (atom, instance) => domHead.contains(atom) && domTail == instance.signature })
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Configurations

case class Config[T <: Value[T]](private val map: Map[Relation, Instance[T]])(implicit num: OneAndZero[T]) extends Map[Relation, Instance[T]] {
  require(map.forall { case (relation, instance) => relation.signature == instance.signature })

  override def apply(relation: Relation): Instance[T] = map.getOrElse(relation, Instance(relation))
  override def get(relation: Relation): Option[Instance[T]] = Some(this(relation))
  override def iterator: Iterator[(Relation, Instance[T])] = map.iterator
  def +(ri: (Relation, Instance[T])): Config[T] = {
    val (relation, instance) = ri
    val newInstance = this(relation) ++ instance
    Config(map + (relation -> newInstance))
  }
  override def +[V >: Instance[T]](kv: (Relation, V)): Map[Relation, V] = map + kv
  override def -(relation: Relation): Config[T] = Config(map - relation)

  def nonEmptySupport: Boolean = map.values.exists(_.nonEmpty)
}

object Config {
  def apply[T <: Value[T]](firstPair: (Relation, Instance[T]), remainingPairs: (Relation, Instance[T])*)(implicit num: OneAndZero[T]): Config[T] = {
    Config((firstPair +: remainingPairs).toMap)
  }
  def apply[T <: Value[T]]()(implicit num: OneAndZero[T]): Config[T] = Config[T](Map[Relation, Instance[T]]())
}
