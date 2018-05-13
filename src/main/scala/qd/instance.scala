package qd

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Instances (of relations / predicates) and configurations

sealed abstract class Instance(val signature: Seq[Domain]) extends (DTuple => Value) {
  require(signature.nonEmpty)

  override def apply(tuple: DTuple): Value = {
    val startTime = System.nanoTime()
    val ans = this match {
      case InstanceBase(domain, map) =>
        require(tuple.length == 1)
        val atom = tuple.head
        require(domain.contains(atom))
        map.getOrElse(atom, Zero)
      case InstanceInd(domHead, _, map) =>
        val atomHead = tuple.head
        require(domHead.contains(atomHead))
        map.get(atomHead).map(_(tuple.tail)).getOrElse(Zero)
    }
    val endTime = System.nanoTime()
    Instance.applyTime += endTime - startTime
    ans
  }

  val support: Seq[(DTuple, Value)] = {
    val startTime = System.nanoTime()
    val ans = this match {
      case InstanceBase(_, map) => map.toSeq.view
        .filter({ case (_, value) => value.nonzero })
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

  def filter(f: Seq[Option[Atom]]): Seq[(DTuple, Value)] = {
    val startTime = System.nanoTime()
    val ans = (this, f.head) match {
      case (InstanceBase(_, map), Some(fh)) =>
        val mfh = map.getOrElse(fh, Zero)
        if (mfh.nonzero) Seq((DTuple(fh), mfh)) else Seq()
      case (InstanceBase(_, map), None) =>
        for ((atom, value) <- map.view.toSeq; if value.nonzero) yield (DTuple(atom), value)
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

  def ++(that: Instance): Instance = {
    val startTime = System.nanoTime()
    val ans = (this, that) match {
      case (InstanceBase(dom1, map1), InstanceBase(dom2, map2)) =>
        require(dom1 == dom2)
        val newMap = for (atom <- map1.keySet ++ map2.keySet;
                          v1 = map1.getOrElse(atom, Zero);
                          v2 = map2.getOrElse(atom, Zero))
          yield atom -> (v1 + v2)
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

  def ++(tvs: Map[DTuple, Value]): Instance = {
    val startTime = System.nanoTime()
    val ans = tvs.foldLeft(this)(_ + _)
    val endTime = System.nanoTime()
    Instance.plusPlusMapTime += endTime - startTime
    ans
  }
  def +(tv: (DTuple, Value)): Instance = {
    val startTime = System.nanoTime()
    val ans = this.add(tv._1, tv._2)
    val endTime = System.nanoTime()
    Instance.plusTime += endTime - startTime
    ans
  }
  def add(tuple: DTuple, value: Value): Instance = this match {
    case InstanceBase(domain, map) =>
      require(tuple.length == 1)
      val atom = tuple.head
      require(domain.contains(atom))
      val oldValue = map.getOrElse(atom, Zero)
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

  def apply(signature: Domain*): Instance = {
    require(signature.nonEmpty)
    if (signature.length == 1) InstanceBase(signature.head, Map())
    else InstanceInd(signature.head, signature.tail, Map())
  }
  def apply(relation: Relation): Instance = Instance(relation.signature:_*)
}

case class InstanceBase(domain: Domain, map: Map[Atom, Value]) extends Instance(Seq(domain)) {
  require(map.keys.forall(domain.contains))
}

case class InstanceInd(domHead: Domain, domTail: Seq[Domain], map: Map[Atom, Instance])
  extends Instance(domHead +: domTail) {
  require(map.forall { case (atom, instance) => domHead.contains(atom) && domTail == instance.signature })
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Configurations

case class Config(private val map: Map[Relation, Instance]) extends Map[Relation, Instance] {
  require(map.forall { case (relation, instance) => relation.signature == instance.signature })

  override def apply(relation: Relation): Instance = map.getOrElse(relation, Instance(relation))
  override def get(relation: Relation): Option[Instance] = Some(this(relation))
  override def iterator: Iterator[(Relation, Instance)] = map.iterator
  def +(ri: (Relation, Instance)): Config = {
    val (relation, instance) = ri
    val newInstance = this(relation) ++ instance
    Config(map + (relation -> newInstance))
  }
  override def +[V >: Instance](kv: (Relation, V)): Map[Relation, V] = map + kv
  override def -(relation: Relation): Config = Config(map - relation)

  def nonEmptySupport: Boolean = map.values.exists(_.nonEmpty)
}

object Config {
  def apply(firstPair: (Relation, Instance), remainingPairs: (Relation, Instance)*): Config = {
    Config((firstPair +: remainingPairs).toMap)
  }
  def apply(): Config = Config(Map[Relation, Instance]())
}
