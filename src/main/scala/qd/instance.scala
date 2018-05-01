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
  override def +[V >: Value](kv: (DTuple, V)): Map[DTuple, V]
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
  val mapd: Map[DTuple, Value] = map.map({ case (atom, value) => DTuple(atom) -> value })
                                    .withDefault(tuple => {
                                      require(tuple.length == 1 && domain.contains(tuple.head));
                                      Zero()
                                    })

  override def get(tuple: DTuple): Option[Value] = mapd.get(tuple)
  override def iterator: Iterator[(DTuple, Value)] = mapd.iterator
  override def +(tv: (DTuple, Value)): Instance = {
    val (tuple, value) = tv
    require(tuple.length == 1)
    val atom = tuple.head
    require(domain.contains(atom))
    InstanceBase(domain, map + (atom -> value))
  }
  override def +[V >: Value](kv: (DTuple, V)): Map[DTuple, V] = mapd + kv
  override def -(tuple: DTuple): Instance = {
    require(tuple.length == 1)
    val atom = tuple.head
    require(domain.contains(atom))
    InstanceBase(domain, map - atom)
  }
  override def ++(that: Map[DTuple, Value]): Instance = ???
  override def --(that: Map[DTuple, Value]): Instance = ???
}

case class InstanceInd(domainHead: Domain, domainTail: Seq[Domain], map: Map[Atom, Instance])
  extends Instance(domainHead +: domainTail) {
  require(map.forall { case (atom, instance) => domainHead.contains(atom) && domainTail == instance.signature })

  override def get(tuple: DTuple): Option[Value] = {
    val t0 = tuple.head
    val tl = DTuple(tuple.tail)
    require(domainHead.contains(t0))
    map.get(t0).flatMap(_.get(tl))
  }

  override def iterator: Iterator[(DTuple, Value)] = ???
  override def +(tv: (DTuple, Value)): Instance = ???
  override def +[V >: Value](kv: (DTuple, V)): Map[DTuple, V] = ???
  override def -(tuple: DTuple): Instance = ???
  override def ++(that: Map[DTuple, Value]): Instance = ???
  override def --(that: Map[DTuple, Value]): Instance = ???
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Configurations

case class Config(private val instances: Map[Relation, Instance]) extends Map[Relation, Instance] {
  require(instances.forall { case (relation, instance) => relation.signature == instance.signature })

  override def apply(relation: Relation): Instance = instances.getOrElse(relation, Instance(relation))
  override def get(relation: Relation): Option[Instance] = Some(this(relation))
  override def iterator: Iterator[(Relation, Instance)] = instances.iterator
  def +(ri: (Relation, Instance)): Config = {
    val (relation, instance) = ri
    val newInstance = this(relation) ++ instance
    Config(instances + (relation -> newInstance))
  }
  override def +[V >: Instance](kv: (Relation, V)): Map[Relation, V] = instances + kv
  override def -(relation: Relation): Config = Config(instances - relation)

  val numTuples: Int = instances.values.map(_.size).sum
  val maxTuple: Map[Relation, DTuple] = instances.mapValues(_.maxTuple)
                                                 .collect { case (tuple, Some(value)) => tuple -> value }
}

object Config {
  def apply(firstPair: (Relation, Instance), remainingPairs: (Relation, Instance)*): Config = {
    Config((firstPair +: remainingPairs).toMap)
  }
  def apply(): Config = Config(Map[Relation, Instance]())
}
