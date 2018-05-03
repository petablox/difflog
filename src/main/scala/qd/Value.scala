package qd

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The semiring of values

case class Value(private val v: Double, prov: Provenance) extends Ordered[Value] {
  require(0.0 <= v && v <= 1.0)
  def zero: Boolean = v == 0.0
  def nonzero: Boolean = v > 0.0

  def +(that: Value): Value = if (this > that) this else that
  def *(that: Value): Value = Value(v * that.v, prov * that.prov)
  override def compare(that: Value): Int = v.compareTo(that.v)

  def toDouble: Double = v
  override def toString: String = v.toString
}

object Value {
  val Zero: Value = Value(0.0, Empty)
  val One: Value = Value(1.0, Empty)
}
