package qd

import qd.util.Contract

case class TokenVec(map: Map[Token, Double]) extends (Lineage => FValue) with Iterable[(Token, Double)] {

  def get(key: Token): Option[Double] = map.get(key)
  def contains(token: Token): Boolean = map.contains(token)
  def keySet: Set[Token] = map.keySet
  override def iterator: Iterator[(Token, Double)] = map.iterator

  override def apply(lineage: Lineage): FValue = lineage match {
    case Empty => implicitly[Semiring[FValue]].One
    case token @ Token(_) => FValue(map(token), token)
    case And(l1, l2) => this(l1) * this(l2)
  }

  def +(tv: (Token, Double)): TokenVec = {
    val (token, value) = tv
    Contract.require(!map.contains(token))
    TokenVec(map + (token -> value))
  }

  def +(that: TokenVec): TokenVec = {
    Contract.require(map.keySet == that.keySet)
    TokenVec(for ((k, v) <- map) yield k -> (v + that.map(k)))
  }

  def -(that: TokenVec): TokenVec = {
    Contract.require(map.keySet == that.keySet)
    TokenVec(for ((k, v) <- map) yield k -> (v - that.map(k)))
  }

  def *(coeff: Double): TokenVec = TokenVec(map.map { case (token, value) => token -> value * coeff })
  def /(denominator: Double): TokenVec = TokenVec(map.map { case (token, value) =>
    val vd = value / denominator
    token -> (if (!vd.isNaN) vd else 0)
  })
  
  def abs: Double = Math.sqrt(map.values.map(v => v * v).sum)
  def unit: TokenVec = this / abs

  def clip(lo: Double, hi: Double): TokenVec = {
    Contract.require(lo <= hi)
    TokenVec(map.map { case (t, v) =>
      val vNew = if (v < lo) lo else if (v > hi) hi else v
      t -> vNew
    })
  }

  def clip(lo: Double, hi: Double, pos: TokenVec): TokenVec = {
    Contract.require(lo <= hi)
    TokenVec(map.map { case (t, v) =>
      val vNew = if (v < lo && pos.map(t) >= lo) lo else if (v > hi && pos.map(t) <= hi) hi else v
      t -> vNew
    })
  }

}

object TokenVec {
  def apply(tokens: Set[Token], f: Token => Double): TokenVec = TokenVec(tokens.map(t => t -> f(t)).toMap)
  def constant(tokens: Set[Token], v: Double): TokenVec = TokenVec(tokens, _ => v)
  def one(tokens: Set[Token]): TokenVec = constant(tokens, 1.0)
  def zero(tokens: Set[Token]): TokenVec = constant(tokens, 0.0)
}
