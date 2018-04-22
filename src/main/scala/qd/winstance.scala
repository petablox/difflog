package qd

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Weighted instances

case class WInstance(relation: Relation, private val map: Map[DTuple, Double]) extends Map[DTuple, Double] {
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

  def +(kv: (DTuple, Double)): WInstance = {
    val (tuple, value) = kv
    require(relation.contains(tuple))
    WInstance(relation, map + (tuple -> Math.max(this(tuple), value)))
  }
  override def +[V >: Double](kv: (DTuple, V)): Map[DTuple, V] = throw new UnsupportedOperationException
  override def -(key: DTuple): WInstance = {
    require(relation.contains(key))
    WInstance(relation, map - key)
  }
  def ++(that: WInstance): WInstance = WInstance(relation, WInstance.max(map, that.map))

  val support: Set[DTuple] = map.filter(_._2 > 0).keySet
  override val size: Int = support.size
  val totalWeight: Double = map.values.sum
  def toInstance(cutoff: Double): Instance = Instance(relation, map.filter(tw => tw._2 >= cutoff).keySet)
}

object WInstance {
  def apply(relation: Relation, firstTuple: (DTuple, Double), remainingTuples: (DTuple, Double)*): WInstance = {
    WInstance(relation, (firstTuple +: remainingTuples).toMap)
  }
  def apply(relation: Relation): WInstance = WInstance(relation, Map[DTuple, Double]())

  def max(map1: Map[DTuple, Double], map2: Map[DTuple, Double]): Map[DTuple, Double] = {
    val (small, large) = if (map1.size <= map2.size) (map1, map2) else (map2, map1)
    var ans = large
    for ((tuple, value) <- small) {
      val newValue = Math.max(ans.getOrElse(tuple, 0.0), value)
      ans = ans + (tuple -> newValue)
    }
    ans
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Weighted configurations

case class WConfig(private val instances: Map[Relation, WInstance]) extends Map[Relation, WInstance] {
  require(instances.forall { case (relation, instance) => relation == instance.relation })

  override def apply(relation: Relation): WInstance = instances.getOrElse(relation, WInstance(relation))
  override def get(relation: Relation): Option[WInstance] = Some(this(relation))
  override def iterator: Iterator[(Relation, WInstance)] = instances.iterator
  def +(ri: (Relation, WInstance)): WConfig = {
    val (relation, instance) = ri
    val newInstance = this(relation) ++ instance
    WConfig(instances + (relation -> newInstance))
  }
  override def +[V >: WInstance](kv: (Relation, V)): Map[Relation, V] = throw new UnsupportedOperationException
  override def -(relation: Relation): WConfig = WConfig(instances - relation)

  val numTuples: Int = instances.values.map(_.size).sum
  val totalWeight: Double = instances.values.map(_.totalWeight).sum
}
