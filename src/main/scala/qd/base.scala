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
    val tuples = set.map(atom => DTuple(atom, atom))
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

  def apply(fields: Atom*): DTuple = {
    val ans = DTuple(fields:_*)
    require(this.contains(ans))
    ans
  }

  def apply(parameters: Parameter*): Literal = Literal(this, parameters:_*)
  def apply(index: Int): Domain = signature(index)
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Instances (of relations / predicates) and configurations

case class Instance(relation: Relation, private val set: Set[DTuple]) extends Set[DTuple] {
  require(set.forall(relation.contains))

  override def contains(tuple: DTuple): Boolean = set.contains(tuple)
  override def iterator: Iterator[DTuple] = set.iterator
  override val size: Int = set.size
  override def +(elem: DTuple): Instance = Instance(relation, set + elem)
  override def -(elem: DTuple): Instance = Instance(relation, set - elem)

  def ++(that: Instance): Instance = Instance(relation, set ++ that.set)
  def ++(that: Iterable[DTuple]): Instance = Instance(relation, set ++ that)
  def --(that: Instance): Instance = Instance(relation, set -- that.set)
  def --(that: Iterable[DTuple]): Instance = Instance(relation, set -- that)

  def toWeightedInstance: WeightedInstance = WeightedInstance(relation, set.map(t => t -> 1.0).toMap)
}

object Instance {
  def apply(relation: Relation, firstTuple: DTuple, remainingTuples: DTuple*): Instance = {
    Instance(relation, (firstTuple +: remainingTuples).toSet)
  }
  def apply(relation: Relation): Instance = Instance(relation, Set[DTuple]())
}

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
  override def +[V >: Instance](kv: (Relation, V)): Map[Relation, V] = throw new UnsupportedOperationException
  override def -(relation: Relation): Config = Config(instances - relation)

  val numTuples: Int = instances.values.map(_.size).sum
}

object Config {
  def apply(firstPair: (Relation, Instance), remainingPairs: (Relation, Instance)*): Config = {
    Config((firstPair +: remainingPairs).toMap)
  }
  def apply(): Config = Config(Map[Relation, Instance]())
  def apply(instances: Instance*): Config = {
    Config(instances.map(instance => instance.relation -> instance).toMap)
  }
  val EMPTY: Config = Config()
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Weighted instances and weighted configurations

case class WeightedInstance(relation: Relation, private val map: Map[DTuple, Double]) extends Map[DTuple, Double] {
  require(map.forall { case (tuple, weight) => relation.contains(tuple) && 0.0 <= weight && weight <= 1.0 })

  override def apply(tuple: DTuple): Double = {
    require(relation.contains(tuple))
    map.getOrElse(tuple, 0.0)
  }
  override def get(tuple: DTuple): Option[Double] = {
    require(relation.contains(tuple))
    map.get(tuple)
  }
  override def iterator: Iterator[(DTuple, Double)] = map.iterator

  def +(kv: (DTuple, Double)): WeightedInstance = {
    val (tuple, value) = kv
    require(relation.contains(tuple))
    WeightedInstance(relation, map + (tuple -> Math.max(this(tuple), value)))
  }
  override def +[V >: Double](kv: (DTuple, V)): Map[DTuple, V] = throw new UnsupportedOperationException
  override def -(key: DTuple): WeightedInstance = {
    require(relation.contains(key))
    WeightedInstance(relation, map - key)
  }

  def ++(that: WeightedInstance): WeightedInstance = {
    ???
  }
  // def ++(that: Iterable[DTuple]): Instance = Instance(relation, set ++ that)
  // def --(that: Instance): Instance = Instance(relation, set -- that.set)
  // def --(that: Iterable[DTuple]): Instance = Instance(relation, set -- that)

  val support: Set[DTuple] = map.filter(_._2 > 0).keySet
  override val size: Int = support.size
  def toInstance(cutoff: Double): Instance = Instance(relation, map.filter(tw => tw._2 >= cutoff).keySet)
}

object WeightedInstance {
  def apply(relation: Relation, firstTuple: (DTuple, Double), remainingTuples: (DTuple, Double)*): WeightedInstance = {
    WeightedInstance(relation, (firstTuple +: remainingTuples).toMap)
  }
  def apply(relation: Relation): WeightedInstance = WeightedInstance(relation, Map[DTuple, Double]())
}

case class WeightedConfig(private val instances: Map[Relation, WeightedInstance])
  extends Map[Relation, WeightedInstance] {
  require(instances.forall { case (relation, instance) => relation == instance.relation })

  override def apply(relation: Relation): WeightedInstance = instances.getOrElse(relation, WeightedInstance(relation))
  override def get(relation: Relation): Option[WeightedInstance] = Some(this(relation))
  override def iterator: Iterator[(Relation, WeightedInstance)] = instances.iterator
  def +(ri: (Relation, WeightedInstance)): Config = {
    val (relation, instance) = ri
    val newInstance = this(relation) ++ instance
    ??? // WeightedConfig(instances + (relation -> newInstance))
  }
  override def +[V >: WeightedInstance](kv: (Relation, V)): Map[Relation, V] = throw new UnsupportedOperationException
  override def -(relation: Relation): WeightedConfig = WeightedConfig(instances - relation)

  val numTuples: Int = instances.values.map(_.size).sum
}