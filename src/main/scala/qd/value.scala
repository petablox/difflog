package qd

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The semiring of values

// This is the set of numbers Vlog = [-infinity, 0], with -infinity explicitly included.
// Note that Vlog = log(Vb), where Vb = [0, 1].
// 1. To add two values is to take their maximum
// 2. To multiply two values is to take their sum. This corresponds to the product in the Vb domain.

sealed abstract class Value(val prov: Provenance) extends Ordered[Value] {
  val zero: Boolean = this match { case Zero => true; case _ => false }
  val nonzero: Boolean = !zero

  def +(that: Value): Value = (this, that) match {
    case (Zero, _) => that
    case (_, Zero) => this
    case (Frac(thisV, thisProv), Frac(thatV, thatProv)) =>
      val ans = if (this > that) (thisV, thisProv, thatProv) else (thatV, thatProv, thisProv)
      Frac(ans._1, ans._2)
  }

  def *(that: Value): Value = (this, that) match {
    case (Zero, _) => Zero
    case (_, Zero) => Zero
    case (Frac(thisV, _), Frac(thatV, _)) => Frac(thisV + thatV, this.prov * that.prov)
  }

  // Over integers, the function abs(this - that) is a simple implementation of the following function.
  override def compare(that: Value): Int = (this, that) match {
    case (Zero, Zero) => 0
    case (Zero, _) => -1
    case (_, Zero) => 1
    case (Frac(thisV, _), Frac(thatV, _)) => thisV.compare(thatV)
  }
}

object Value {
  def apply(v: BigDecimal, prov: Provenance): Frac = Frac(v, prov)
}

case object Zero extends Value(Empty) {
  override def toString: String = "-inf"
}

case class Frac(private val v: BigDecimal, override val prov: Provenance) extends Value(prov) {
  require(v <= 0)
  override def toString: String = v.toString
}

object One extends Frac(0, Empty)
