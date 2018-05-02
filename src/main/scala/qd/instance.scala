package qd

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Instances (of relations / predicates) and configurations

sealed abstract class Instance(val signature: Seq[Domain]) extends Map[DTuple, Value] {
  require(signature.nonEmpty)

  lazy val support: Set[DTuple] = this.filter({ case (_, value) => value.nonzero }).keySet
  override def size: Int = support.size
  lazy val maxTuple: Option[DTuple] = {
    if (support.nonEmpty) {
      val maxValue = this.values.max
      Some(this.find({ case (_, value) => value >= maxValue }).get._1)
    } else None
  }

  override def contains(tuple: DTuple): Boolean
  override def get(tuple: DTuple): Option[Value]
  override def iterator: Iterator[(DTuple, Value)]

  def +(tv: (DTuple, Value)): Instance
  override def +[V >: Value](kv: (DTuple, V)): Map[DTuple, V] = throw new UnsupportedOperationException
  override def -(tuple: DTuple): Instance
  def ++(that: Map[DTuple, Value]): Instance
  def --(that: Map[DTuple, Value]): Instance
}

object Instance {
  def apply(signature: Domain*): Instance = {
    require(signature.nonEmpty)
    if (signature.length == 1) InstanceBase(signature.head, Map())
    else InstanceInd(signature.head, signature.tail, Map())
  }
  def apply(relation: Relation): Instance = Instance(relation.signature:_*)
}

case class InstanceBase(domain: Domain, map: Map[Atom, Value]) extends Instance(Seq(domain)) {
  require(map.keys.forall(domain.contains))

  val mapd: Map[Atom, Value] = map.withDefault(atom => { require(domain.contains(atom)); Zero })
  def toAtom(tuple: DTuple): Atom = {
    require(tuple.length == 1)
    tuple.head
  }

  override def contains(tuple: DTuple): Boolean = mapd(toAtom(tuple)).nonzero
  override def get(tuple: DTuple): Option[Value] = mapd.get(toAtom(tuple))
  override def iterator: Iterator[(DTuple, Value)] = mapd.iterator.map { case (atom, value) => (DTuple(atom), value)}

  override def +(tv: (DTuple, Value)): Instance = {
    val (tuple, value) = tv
    require(tuple.length == 1)
    val atom = tuple.head
    val oldValue = mapd(atom)
    if (oldValue <= value) {
      require(domain.contains(atom))
      InstanceBase(domain, map + (atom -> value))
    } else this
  }
  override def -(tuple: DTuple): Instance = {
    require(tuple.length == 1)
    val atom = tuple.head
    require(domain.contains(atom))
    InstanceBase(domain, map - atom)
  }

  override def ++(that: Map[DTuple, Value]): Instance = {
    val thata = that.map { case (tuple, value) =>
      require(tuple.length == 1)
      val atom = tuple.head
      require(domain.contains(atom))
      atom -> value
    }
    val thatd = thata.withDefaultValue(Zero)
    val newMap = for (atom <- this.keySet ++ thatd.keySet) yield atom -> mapd(atom) + thatd(atom)
    ???
  }
  override def --(that: Map[DTuple, Value]): Instance = {
    val thatd = that.map({ case (tuple, value) =>
      require(tuple.length == 1)
      val atom = tuple.head
      require(domain.contains(atom))
      atom -> value
    }).withDefaultValue(Zero())
    InstanceBase(domain, map.filter { case (key, value) => value > thatd(key) })
  }
}

case class InstanceInd(domainHead: Domain, domainTail: Seq[Domain], map: Map[Atom, Instance])
  extends Instance(domainHead +: domainTail) {
  require(map.forall { case (atom, instance) => domainHead.contains(atom) && domainTail == instance.signature })
  val mapd: Map[Atom, Instance] = mapd.withDefault { atom =>
    require(domainHead.contains(atom));
    Instance(domainTail:_*)
  }

  override def contains(tuple: DTuple): Boolean = get(tuple).getOrElse(Zero).nonzero
  override def get(tuple: DTuple): Option[Value] = mapd(tuple.head).get(tuple.tail)
  override def iterator: Iterator[(DTuple, Value)] = {
    map.map({ case (atom, ia) => atom -> ia.iterator
                                           .map { case (tuple, value) => (atom +: tuple) -> value } })
       .values.flatten.toIterator
  }

  override def +(tv: (DTuple, Value)): Instance = {
    val (tuple, value) = tv
    val head = tuple.head
    require(domainHead.contains(head))
    val sub = mapd(head)
    val newSub = sub + (tuple.tail -> value)
    val newMap = map + (head -> newSub)
    InstanceInd(domainHead, domainTail, newMap)
  }
  override def -(tuple: DTuple): Instance = {
    val head = tuple.head
    require(domainHead.contains(head))
    if (map.contains(head)) InstanceInd(domainHead, domainTail, map + (head -> (map(head) - tuple.tail)))
    else this
  }

  override def ++(that: Map[DTuple, Value]): Instance = ???
  override def --(that: Map[DTuple, Value]): Instance = ???
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

  val numTuples: Int = map.values.map(_.size).sum
  val maxTuple: Map[Relation, DTuple] = map.mapValues(_.maxTuple)
                                           .collect { case (tuple, Some(value)) => tuple -> value }
}

object Config {
  def apply(firstPair: (Relation, Instance), remainingPairs: (Relation, Instance)*): Config = {
    Config((firstPair +: remainingPairs).toMap)
  }
  def apply(): Config = Config(Map[Relation, Instance]())
}
