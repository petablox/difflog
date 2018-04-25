package qd

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The semiring of values

// This is the set of numbers Vlog = [-infinity, 0], with -infinity explicitly included.
// Note that Vlog = log(Vb), where Vb = [0, 1].
// 1. To add two values is to take their maximum
// 2. To multiply two values is to take their sum. This corresponds to the product in the Vb domain.

sealed abstract class Value(val prov: Provenance) extends Ordered[Value] {
  def +(that: Value): Value
  def *(that: Value): Value
  val zero: Boolean
  val nonzero: Boolean = !zero
}

case class Zero(override val prov: Provenance) extends Value(prov) {
  override def +(that: Value): Value = that match {
    case Zero(thatProv) => Zero(thatProv)
    case Frac(thatV, thatProv) => Frac(thatV, thatProv)
  }
  override def *(that: Value): Value = Zero(prov * that.prov)
  override val zero: Boolean = true
  override def compare(that: Value): Int = that match {
    case Zero(_) => 0
    case Frac(_, _) => -1
  }
  override def toString: String = "-inf"
}

object Zero {
  val sentinel: Zero = Zero(Empty)
  def apply(): Zero = sentinel
}

case class Frac(private val v: BigDecimal, override val prov: Provenance) extends Value(prov) {
  require(v <= 0)
  override def +(that: Value): Value = that match {
    case Zero(_) => Frac(v, prov)
    case Frac(thatV, thatProv) =>
      val ans = if (v > thatV) (v, prov, thatProv) else (thatV, thatProv, prov)
      Frac(ans._1, ans._2)
  }
  override def *(that: Value): Value = that match {
    case Zero(thatProv) => Zero(prov * thatProv)
    case Frac(thatV, thatProv) => Frac(v + thatV, prov * thatProv)
  }
  override val zero: Boolean = false
  override def compare(that: Value): Int = that match {
    case Zero(_) => 1
    case Frac(vThat, _) => v.compare(vThat)
  }
  override def toString: String = v.toString
}

object Value {
  def apply(v: BigDecimal, prov: Provenance): Frac = Frac(v, prov)
  def One(prov: Provenance) = Frac(0, prov)
  val One: Frac = One(Empty)
}
