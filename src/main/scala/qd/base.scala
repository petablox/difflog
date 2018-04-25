package qd

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Atoms and domains

case class Atom(t: Any) {
  override def toString: String = s"<$t>"
}

case class Domain(name: Any, private val set: Set[Atom]) extends Set[Atom] {
  override def contains(atom: Atom): Boolean = set.contains(atom)
  override def empty: Domain = Domain("Empty")
  override def foreach[U](f: Atom => U): Unit = set.foreach(f)
  override def iterator: Iterator[Atom] = set.iterator
  override val size: Int = set.size
  override def +(atom: Atom): Set[Atom] = Domain(s"$name + $atom", set + atom)
  override def -(atom: Atom): Set[Atom] = Domain(s"$name - $atom", set - atom)
  override def toString: String = s"$name[${set.mkString(", ")}]"

  def equalityRelation: Instance = {
    val relation = Relation(s"Eq$name", this, this)
    val tuples = set.map(atom => DTuple(atom, atom) -> Value.One).toMap
    Instance(relation, tuples)
  }
}

object Domain {
  def apply(name: Any, firstAtom: Atom, remainingAtoms: Atom*): Domain = {
    Domain(name, (firstAtom +: remainingAtoms).toSet)
  }
  def apply(name: Any): Domain = {
    Domain(name, Set[Atom]())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Tuples and relations (aka predicates)

case class DTuple(private val fields: Atom*) extends Seq[Atom] {
  override def apply(index: Int): Atom = fields(index)
  override def iterator: Iterator[Atom] = fields.iterator
  override val length: Int = fields.length
  override def toString: String = s"(${fields.mkString(", ")})"
}

case class Relation(name: Any, signature: Domain*) {
  def contains(tuple: DTuple): Boolean = {
    signature.length == tuple.length &&
    signature.zip(tuple).forall { case (domain, field) => domain.contains(field) }
  }
  val arity: Int = signature.length
  override def toString: String = s"$name(${signature.map(_.name).mkString(", ")})"

  def apply(index: Int): Domain = signature(index)
  def apply(fields: Atom*): DTuple = {
    val ans = DTuple(fields:_*)
    require(this.contains(ans))
    ans
  }
  def apply(parameters: Parameter*): Literal = Literal(Zero(), this, parameters:_*)
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Instances (of relations / predicates) and configurations

case class Instance(relation: Relation, private val map: Map[DTuple, Value]) extends Map[DTuple, Value] {
  require(map.keys.forall(relation.contains))

  private val mapd = map.withDefault(tuple => { require(relation.contains(tuple)); Zero() })
  val support: Set[DTuple] = map.filter(_._2.nonzero).keySet
  override val size: Int = support.size
  val maxTuple: Option[DTuple] = {
    if (support.nonEmpty) {
      val maxValue = map.values.max
      Some(map.find({ case (_, value) => value >= maxValue }).get._1)
    } else None
  }

  override def apply(tuple: DTuple): Value = mapd(tuple)
  override def contains(tuple: DTuple): Boolean = support.contains(tuple)
  override def get(tuple: DTuple): Option[Value] = mapd.get(tuple)
  override def iterator: Iterator[(DTuple, Value)] = map.iterator
  def +(tv: (DTuple, Value)): Instance = {
    val (tuple, value) = tv
    val newValue = mapd(tuple) + value
    Instance(relation, map + (tuple -> newValue))
  }
  override def +[V >: Value](kv: (DTuple, V)): Map[DTuple, V] = map + kv
  override def -(tuple: DTuple): Instance = Instance(relation, map - tuple)

  def ++(that: Map[DTuple, Value]): Instance = Instance(relation, Instance.merge(map, that))
  def --(that: Map[DTuple, Value]): Instance = {
    val thatd = that.withDefaultValue(Zero())
    val delta = map.filter({ case (tuple, value) => value > thatd(tuple) })
    Instance(relation, delta)
  }
}

object Instance {
  def apply(relation: Relation, firstTuple: (DTuple, Value), remainingTuples: (DTuple, Value)*): Instance = {
    Instance(relation, (firstTuple +: remainingTuples).toMap)
  }
  def apply(relation: Relation): Instance = Instance(relation, Map[DTuple, Value]())

  def merge(map1: Map[DTuple, Value], map2: Map[DTuple, Value]): Map[DTuple, Value] = {
    val (small, large) = if (map1.size < map2.size) (map1, map2) else (map2, map1)
    var ans = large.withDefaultValue(Zero())
    for ((tuple, value) <- small) {
      val newValue = ans(tuple) + value
      ans = ans + (tuple -> newValue)
    }
    ans
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Configurations

case class Config(private val instances: Map[Relation, Instance]) extends Map[Relation, Instance] {
  require(instances.forall { case (relation, instance) => relation == instance.relation })

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
  def apply(instances: Instance*): Config = {
    Config(instances.map(instance => instance.relation -> instance).toMap)
  }
}
