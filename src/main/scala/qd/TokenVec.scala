package qd

import Semiring.FValueSemiringObj

import scala.util.Random

case class TokenVec(map: Map[Token, Double]) extends (Lineage => FValue) with Iterable[(Token, Double)] {

  implicit val vs: FValueSemiring = FValueSemiringObj

  def get(key: Token): Option[Double] = map.get(key)
  def contains(token: Token): Boolean = map.contains(token)
  def keySet: Set[Token] = map.keySet
  override def iterator: Iterator[(Token, Double)] = map.iterator

  override def apply(lineage: Lineage): FValue = lineage match {
    case Empty => vs.One
    case token @ Token(_) => FValue(map(token), token)
    case And(l1, l2) => this(l1) * this(l2)
  }

  def apply(p: Program[FValue]): Program[FValue] = {
    val newRules = p.rules.map(r => Rule(this(r.coeff.l), r.head, r.body))
    Program(p.name, newRules)
  }

  def +(tv: (Token, Double)): TokenVec = {
    val (token, value) = tv
    require(!map.contains(token))
    TokenVec(map + (token -> value))
  }

  def +(that: TokenVec): TokenVec = {
    require(map.keySet == that.keySet)
    TokenVec(for ((k, v) <- map) yield k -> (v + that.map(k)))
  }

  def -(that: TokenVec): TokenVec = {
    require(map.keySet == that.keySet)
    TokenVec(for ((k, v) <- map) yield k -> (v - that.map(k)))
  }

  def *(coeff: Double): TokenVec = TokenVec(map.map { case (token, value) => token -> value * coeff })
  def /(denom: Double): TokenVec = TokenVec(map.map { case (token, value) =>
    val vd = value / denom
    token -> (if (!vd.isNaN) vd else 0)
  })

  //def abs: Double = m.values.map(v => v * v).sum
  def abs: Double = Math.sqrt(map.values.map(v => v * v).sum)
  def unit: TokenVec = this / abs

  def limitUpper(v: Double): TokenVec = TokenVec(map.mapValues(x => Math.min(v, x)))

  def limitUpper(maxV: Double, pos: TokenVec): TokenVec = {
    val mp = map.map { case (token, oldV) => token -> (if (maxV >= oldV || pos.map(token) > maxV) oldV else maxV) }
    TokenVec(mp)
  }

  def limitLower(v: Double): TokenVec = TokenVec(map.mapValues(x => Math.max(v, x)))

  def limitLower(minV: Double, pos: TokenVec): TokenVec = {
    val mp = map.map { case (token, oldV) => token -> (if (minV <= oldV || pos.map(token) < minV) oldV else minV) }
    TokenVec(mp)
  }

}

object TokenVec {
  def apply(tokens: Set[Token], f: Token => Double): TokenVec = TokenVec(tokens.map(t => t -> f(t)).toMap)
  def constant(tokens: Set[Token], v: Double): TokenVec = TokenVec(tokens, _ => v)
  def one(tokens: Set[Token]): TokenVec = constant(tokens, 1.0)
  def zero(tokens: Set[Token]): TokenVec = constant(tokens, 0.0)
}
