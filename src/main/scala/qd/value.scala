package qd

package value {

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
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

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // The semiring of values

  case class Value(private val v: Double, prov: Provenance) extends Ordered[Value] {
    require(0.0 <= v && v <= 1.0)
    def zero: Boolean = v == 0.0
    def nonzero: Boolean = v > 0.0

    def +(that: Value): Value = if (this > that) this else that
    def *(that: Value): Value = Value(v * that.v, prov * that.prov)
    override def compare(that: Value): Int = v.compareTo(that.v)

    def toDouble: Double = v
    override def toString: String = s"$v"
  }

  object Value {
    def apply(p: Provenance, v: Token => Value): Value = p match {
      case Empty => Zero
      case t @ Token(_) => v(t)
      case ProvenanceProduct(p1, p2) => Value(p1, v) * Value(p2, v)
    }
  }

  object Zero extends Value(0.0, Empty)
  object One extends Value(1.0, Empty)

}
