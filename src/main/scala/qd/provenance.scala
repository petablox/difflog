package qd

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The space of provenance tokens

sealed abstract class Provenance {
  def *(that: Provenance): Provenance = (this, that) match {
    case (Empty, _) => that
    case (_, Empty) => this
    case (_, _) => ProvenanceProduct(this, that)
  }

  def toSeq: Seq[Token] = this match {
    case Empty => Seq()
    case t @ Token(_) => Seq(t)
    case ProvenanceProduct(p1, p2) => p1.toSeq ++ p2.toSeq
  }

  override def toString: String = this match {
    case Empty => "Empty"
    case Token(name) => s"$name"
    case ProvenanceProduct(p1, p2) => s"$p1$p2*"
  }
}

case object Empty extends Provenance
case class Token(name: Any) extends Provenance
case class ProvenanceProduct private(p1: Provenance, p2: Provenance) extends Provenance
