package qd

//import java.util.Set

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The semiring of values

trait Value[T <: Value[T]] extends Ordered[T] {
  def +(that: T): T
  def *(that: T): T
  //def compare(that: T): Int
  def isZero: Boolean
  def isNonZero: Boolean = !isZero
  def getUnderlying : T
}

trait OneAndZero[T <: Value[T]] {
  def One: T
  def Zero: T
}

case class FValue(private val v: Double, prov: Provenance) extends Value[FValue] {
  require(0.0 <= v && v <= 1.0)
  override def isZero: Boolean = v == 0.0
  //def nonzero: Boolean = v > 0.0

  override def +(that: FValue): FValue = if (this > that) this else that
  override def *(that: FValue): FValue = FValue(v * that.v, prov * that.prov)
  override def compare(that: FValue): Int = v.compare(that.v)

  def toDouble: Double = v
  override def toString: String = s"$v"

  override def getUnderlying: FValue = this
}

object FValue {
  implicit object ValueHasOneAndZero extends OneAndZero[FValue] {
    def One = FValue(1.0, Empty)
    def Zero = FValue(0.0, Empty)
  }

  def apply(p: Provenance, v: Token => FValue): FValue = p match {
    case Empty => One
    case t @ Token(_) => v(t)
    case ProvenanceProduct(p1, p2) => FValue(p1, v) * FValue(p2, v)
  }

  val Zero: FValue = FValue(0.0, Empty)
  val One: FValue = FValue(1.0, Empty)
}

case class DValue(r: Set[Set[Int]]) extends Value[DValue] {
  def isSubsetOfOne(r : Set[Set[Int]], s : Set[Int]): Boolean = r.exists((x : Set[Int]) => (s != x) && (x subsetOf s))

  override def +(that : DValue) : DValue = {
    val union = r.union(that.r)
    val filtered = union.filter(!isSubsetOfOne(union, _))
    DValue(filtered)
    //val self_filtered = r.filter(!isSubsetOfOne(that.r, _))
    //DValue(self_filtered.union(that.r.filter(!isSubsetOfOne(r, _))))
  }

  override def *(that : DValue) : DValue = {
    val sets = for (s1 <- r; s2 <- that.r) yield s1.union(s2)
    val filtered = sets.filter(!isSubsetOfOne(sets, _))
    DValue(filtered)
  }

  def canonize() = {
    DValue(r.filter(!isSubsetOfOne(r, _)))
  }

  override def compare(that : DValue) : Int = if (r == that.r) 0 else if (this + that == that) -1 else 1

  override def isZero : Boolean = r.isEmpty

  def One = DValue(Set(Set()))
  def Zero = DValue(Set())

  override def getUnderlying: DValue = this
}

object DValue {
  implicit object ValueHasOneAndZero extends OneAndZero[DValue] {
    def One = DValue(Set(Set()))
    def Zero = DValue(Set())
  }

  def One = DValue(Set(Set()))
  def Zero = DValue(Set())
}