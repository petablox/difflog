package qd

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Atoms and domains

case class Atom(t: Any) {
  override def toString: String = s"<$t>"
}

case class Domain(name: Any, allAtoms: Set[Atom]) {
  def contains(atom: Atom): Boolean = allAtoms.contains(atom)
  val size: Int = allAtoms.size

  def equalityRelation: Instance = {
    val relation = Relation(s"Eq$name", this, this)
    val tuples = allAtoms.map(atom => DTuple(atom, atom))
    Instance(relation, tuples)
  }

  override def toString: String = s"$name[${allAtoms.mkString(", ")}]"
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

case class DTuple(fields: Atom*) {
  val length: Int = fields.length
  def apply(index: Int): Atom = fields(index)
  override def toString: String = s"(${fields.mkString(", ")})"
}

case class Relation(name: Any, signature: Domain*) {
  def contains(tuple: DTuple): Boolean = {
    signature.length == tuple.length && signature.zip(tuple.fields).forall(df => df._1.contains(df._2))
  }
  val numFields: Int = signature.length
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

case class Instance(relation: Relation, tuples: Set[DTuple]) extends Set[DTuple] {
  require(tuples.forall(relation.contains))
  def contains(tuple: DTuple): Boolean = tuples.contains(tuple)
  val numTuples: Int = tuples.size
  def toWeightedInstance: WeightedInstance = WeightedInstance(relation, tuples.map(t => t -> 1.0).toMap)

  def ++(that: Instance): Instance = {
    require(relation == that.relation)
    Instance(relation, tuples ++ that.tuples)
  }

  def ++(that: Iterable[DTuple]): Instance = {
    require(that.forall(relation.contains))
    Instance(relation, tuples ++ that)
  }

  def --(that: Instance): Instance = {
    require(relation == that.relation)
    Instance(relation, tuples -- that.tuples)
  }

  def --(that: Iterable[DTuple]): Instance = {
    require(that.forall(relation.contains))
    Instance(relation, tuples -- that)
  }

  override def +(elem: DTuple): Instance = Instance(relation, tuples + elem)
  override def -(elem: DTuple): Instance = Instance(relation, tuples - elem)
  override def iterator: Iterator[DTuple] = tuples.iterator
}

object Instance {
  def apply(relation: Relation, firstTuple: DTuple, remainingTuples: DTuple*): Instance = {
    Instance(relation, (firstTuple +: remainingTuples).toSet)
  }
  def apply(relation: Relation): Instance = Instance(relation, Set[DTuple]())
}

case class Config(instances: Map[Relation, Instance]) {
  require(instances.forall { case (relation, instance) => relation == instance.relation })
  val numTuples: Int = instances.values.map(_.numTuples).sum
  def apply(relation: Relation): Instance = instances(relation)
  def getOrElse(relation: Relation, default: => Instance): Instance = instances.getOrElse(relation, default)
  def withDefault(default: Relation => Instance): Config = {
    val ans = Config(instances.withDefault(default))
    assert(numTuples == ans.numTuples)
    ans
  }
  def +(si: (Relation, Instance)): Config = Config(instances + si)
}

object Config {
  def apply(firstPair: (Relation, Instance), remainingPairs: (Relation, Instance)*): Config = {
    Config((firstPair +: remainingPairs).toMap)
  }
  def apply(): Config = Config(Map[Relation, Instance]())
  def apply(instances: Instance*): Config = {
    Config(instances.map(instance => instance.relation -> instance).toMap)
  }
  val EMPTY: Config = Config().withDefault(relation => Instance(relation))
}

case class WeightedInstance(relation: Relation, tuples: Map[DTuple, Double]) {
  require(tuples.forall { case (tuple, weight) => relation.contains(tuple) && 0.0 <= weight && weight <= 1.0 })
  def valueOf(tuple: DTuple): Double = {
    require(relation.contains(tuple))
    tuples.getOrElse(tuple, 0.0)
  }
  def toInstance(cutoff: Double): Instance = Instance(relation, tuples.filter(tw => tw._2 >= cutoff).keySet)
}
