package qd

import scala.collection.immutable._

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Domains, Parameters, Constants, and Variables

case class Domain(name: Any) {
  override def toString: String = s"$name"
  override val hashCode: Int = ("D", name).hashCode()
}

sealed abstract class Parameter {
  def name: Any
  def domain: Domain
  override def toString: String = s"$name"
}
case class Constant(name: Any, domain: Domain) extends Parameter {
  override val hashCode: Int = ("C", name, domain).hashCode()
}
case class Variable(name: Any, domain: Domain) extends Parameter {
  override val hashCode: Int = ("V", name, domain).hashCode()
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Relations and Tuples

case class Relation(name: Any, signature: Seq[Domain]) {
  def arity: Int = signature.length
  def apply(fields: Seq[Parameter]): Literal = Literal(this, fields)
  override def toString: String = s"$name(${signature.mkString(", ")})"
  override val hashCode: Int = ("R", name, signature).hashCode()
}

case class DTuple(fields: Seq[Constant]) extends Seq[Constant] {
  lazy val signature: Seq[Domain] = fields.map(_.domain)
  def arity: Int = fields.length

  override def apply(index: Int): Constant = fields(index)
  override def iterator: Iterator[Constant] = fields.iterator
  override val length: Int = fields.length
  override val hashCode: Int = fields.hashCode()
  override def head: Constant = fields.head
  override def tail: DTuple = DTuple(fields.tail)

  def +:(field: Constant): DTuple = DTuple(field +: fields)
  def :+(field: Constant): DTuple = DTuple(fields :+ field)

  override def toString: String = fields.mkString("(", ", ", ")")
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Literals and Rules

case class Literal(relation: Relation, fields: Seq[Parameter]) {
  require(relation.signature == fields.map(_.domain))
  val variables: Set[Variable] = fields.collect({ case v: Variable => v }).toSet
  override def toString: String = s"${relation.name}(${fields.mkString(", ")})"
}

case class Rule[T <: Value[T]](coeff: T, head: Literal, body: Seq[Literal]) {
  val bodySet: Set[Literal] = body.toSet
  val bodyDistinct: Seq[Literal] = body.distinct

  val variables: Set[Variable] = bodySet.flatMap(_.variables)
  require(head.variables.subsetOf(variables))

  lazy val relations: Set[Relation] = bodySet.map(_.relation) + head.relation
  lazy val domains: Set[Domain] = bodySet.flatMap(_.fields.map(_.domain)) ++ head.fields.map(_.domain)

  lazy val valency: Int = Range(1, bodyDistinct.size).maxBy { i =>
    val left = bodyDistinct.take(i).toSet
    val right = bodyDistinct.drop(i).toSet
    val leftVars = left.flatMap(_.variables)
    val rightVars = right.flatMap(_.variables) ++ head.variables
    leftVars.intersect(rightVars).size
  }

  override def toString: String = {
    val sortedBody = body.map(_.toString).toList.sorted
    s"$coeff: $head :- ${sortedBody.mkString(", ")}."
  }
}
