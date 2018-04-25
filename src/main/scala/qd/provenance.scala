package qd

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The space of provenance tokens

sealed abstract class Provenance {
  def toSeq: Seq[Token]
  def *(that: Provenance): Provenance = (this, that) match {
    case (Empty, _) => that
    case (_, Empty) => this
    case (_, _) => ProvenanceProduct(this, that)
  }
}

case object Empty extends Provenance {
  override def toSeq: Seq[Token] = Seq()
  override def toString: String = "Empty"
}

case class Token(name: Any) extends Provenance {
  override def toSeq: Seq[Token] = Seq(this)
  override def toString: String = name.toString
}

case class ProvenanceProduct private(p1: Provenance, p2: Provenance) extends Provenance {
  override def toSeq: Seq[Token] = p1.toSeq ++ p2.toSeq
  override def toString: String = s"$p1$p2*"
}
