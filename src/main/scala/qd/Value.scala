package qd

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The semiring of values

case class Value(private val v: Double, prov: Provenance) extends Ordered[Value] {
  require(0.0 <= v && v <= 1.0)
  def zero: Boolean = v == 0.0
  def nonzero: Boolean = v > 0.0

  def +(that: Value): Value = if (this > that) this else that
  def *(that: Value): Value = Value(v * that.v, prov * that.prov)
  override def compare(that: Value): Int = v.compare(that.v)

  def toDouble: Double = v
  override def toString: String = s"$v"
}

object Value {
  def apply(p: Provenance, v: Token => Value): Value = p match {
    case Empty => One
    case t @ Token(_) => v(t)
    case ProvenanceProduct(p1, p2) => Value(p1, v) * Value(p2, v)
  }
}

object Zero extends Value(0.0, Empty)
object One extends Value(1.0, Empty)