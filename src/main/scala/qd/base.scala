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
    val tuples = set.map(atom => DTuple(atom, atom) -> 1.0).toMap
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
  def apply(parameters: Parameter*): Literal = Literal(0.0, this, parameters:_*)
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Instances (of relations / predicates) and configurations

case class Instance(relation: Relation, private val map: Map[DTuple, Double]) extends Map[DTuple, Double] {
  require(map.forall { case (tuple, weight) => relation.contains(tuple) && 0.0 <= weight && weight <= 1.0 })

  private val mapd = map.withDefault(tuple => { require(relation.contains(tuple)); 0.0 })
  val support: Set[DTuple] = map.filter(_._2 > 0).keySet
  override val size: Int = support.size
  val totalWeight: Double = map.values.sum

  override def apply(tuple: DTuple): Double = mapd(tuple)
  override def contains(tuple: DTuple): Boolean = mapd(tuple) > 0.0
  override def get(tuple: DTuple): Option[Double] = mapd.get(tuple)
  override def iterator: Iterator[(DTuple, Double)] = map.iterator
  def +(tv: (DTuple, Double)): Instance = {
    val (tuple, value) = tv
    val newValue = Instance.merge(mapd(tuple), value)
    Instance(relation, map + (tuple -> newValue))
  }
  override def +[V >: Double](kv: (DTuple, V)): Map[DTuple, V] = map + kv
  override def -(tuple: DTuple): Instance = Instance(relation, map - tuple)

  def ++(that: Instance): Instance = Instance(relation, Instance.merge(map, that.map))
  def ++(that: Map[DTuple, Double]): Instance = Instance(relation, Instance.merge(map, that))
  def --(that: Instance): Instance = Instance(relation, map.filter { case (tuple, value) => value > that(tuple) })
  def --(that: Map[DTuple, Double]): Instance = {
    Instance(relation, map.filter { case (tuple, value) => value > that.getOrElse(tuple, 0.0) })
  }
}

object Instance {
  def apply(relation: Relation, firstTuple: (DTuple, Double), remainingTuples: (DTuple, Double)*): Instance = {
    Instance(relation, (firstTuple +: remainingTuples).toMap)
  }
  def apply(relation: Relation): Instance = Instance(relation, Map[DTuple, Double]())

  def merge(value1: Double, value2: Double): Double = Math.max(value1, value2)
  def merge(map1: Map[DTuple, Double], map2: Map[DTuple, Double]): Map[DTuple, Double] = {
    val (small, large) = if (map1.size < map2.size) (map1, map2) else (map2, map1)
    var ans = large
    for ((tuple, value) <- small) {
      val newValue = merge(ans.getOrElse(tuple, 0.0), value)
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
  val totalWeight: Double = instances.values.map(_.totalWeight).sum
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
