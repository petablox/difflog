package qd

import qd.util.Multiset

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Lineages of Values
// It is the set of tokens, _all_ of which were involved in the derivation of a conclusion
// It is only intended to make sense for FValues

sealed abstract class Lineage {
  def *(that: Lineage): Lineage = (this, that) match {
    case (Empty, _) => that
    case (_, Empty) => this
    case (_, _) => And(this, that)
  }

  def toSeq: Seq[Token] = this match {
    case Empty => Seq()
    case t @ Token(_) => Seq(t)
    case And(l1, l2) => l1.toSeq ++ l2.toSeq
  }

  def toMultiset: Multiset[Token] = toSeq.foldLeft(Multiset[Token]())(_ + _)

  override def toString: String = this match {
    case Empty => "Empty"
    case Token(name) => s"$name"
    case And(l1, l2) => s"$l1$l2*"
  }
}

case object Empty extends Lineage
case class Token(name: Any) extends Lineage
case class And(l1: Lineage, l2: Lineage) extends Lineage
