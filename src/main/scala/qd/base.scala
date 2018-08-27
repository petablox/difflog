package qd

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Domains, Parameters, Constants, and Variables

case class Domain(name: Any) {
  override def toString: String = s"$name"
}

sealed abstract class Parameter {
  def name: Any
  def domain: Domain
  override def toString: String = s"$name"
}
case class Constant(name: Any, domain: Domain) extends Parameter
case class Variable(name: Any, domain: Domain) extends Parameter

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Relations and Tuples

case class Relation(name: Any, signature: Domain*) {
  def arity: Int = signature.length
  def apply(fields: Parameter*): Literal = Literal(this, fields:_*)
  override def toString: String = s"$name(${signature.mkString(", ")})"
}

case class DTuple(fields: Constant*) extends Seq[Constant] {
  val signature: Seq[Domain] = fields.map(_.domain)
  def arity: Int = fields.length

  override def apply(index: Int): Constant = fields(index)
  override def iterator: Iterator[Constant] = fields.iterator
  override val length: Int = fields.length
  override def hashCode(): Int = fields.hashCode()
  override def head: Constant = fields.head
  override def tail: DTuple = DTuple(fields.tail:_*)

  def +:(field: Constant): DTuple = DTuple(field +: fields:_*)
  def :+(field: Constant): DTuple = DTuple(fields :+ field:_*)

  override def toString: String = fields.mkString("(", ", ", ")")
}
