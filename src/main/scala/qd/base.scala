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
    set.foldLeft(Instance(this, this)) { case (instance, atom) => instance + (DTuple(atom, atom) -> Value.One )}
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
  require(fields.nonEmpty)
  override def apply(index: Int): Atom = fields(index)
  override def iterator: Iterator[Atom] = fields.iterator
  override val length: Int = fields.length
  override def tail: DTuple = DTuple(fields.tail:_*)
  def +:(field: Atom): DTuple = DTuple(field +: fields:_*)
  def :+(field: Atom): DTuple = DTuple(fields :+ field:_*)
  override def toString: String = s"(${fields.mkString(", ")})"
}

case class Relation(name: Any, signature: Domain*) {
  require(signature.nonEmpty)

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
  def apply(parameters: Parameter*): Literal = Literal(Value.Zero, this, parameters:_*)

  def populate: Set[DTuple] = {
    def p(sign: Seq[Domain]): Set[DTuple] = {
      if (sign.length == 1) {
        val domain = sign.head
        domain.map(atom => DTuple(atom))
      } else {
        val domHead = sign.head
        for (tail <- p(sign.tail); atom <- domHead) yield atom +: tail
      }
    }
    p(signature)
  }
}
