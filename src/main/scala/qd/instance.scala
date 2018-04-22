package qd

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Instances (of relations / predicates)

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

  def toWeightedInstance: WInstance = WInstance(relation, set.map(t => t -> 1.0).toMap)
}

object Instance {
  def apply(relation: Relation, firstTuple: DTuple, remainingTuples: DTuple*): Instance = {
    Instance(relation, (firstTuple +: remainingTuples).toSet)
  }
  def apply(relation: Relation): Instance = Instance(relation, Set[DTuple]())
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
