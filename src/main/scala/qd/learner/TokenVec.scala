package qd
package learner

import scala.util.Random

case class TokenVec(map: Map[Token, Double]) extends Map[Token, Double] {
  override def +[V >: Double](kv: (Token, V)): Map[Token, V] = map + kv
  override def get(key: Token): Option[Double] = map.get(key)
  override def iterator: Iterator[(Token, Double)] = map.iterator
  override def -(key: Token): TokenVec = TokenVec(map - key)

  def +(that: TokenVec): TokenVec = {
    require(map.keySet == that.keySet)
    TokenVec(for ((k, v) <- map) yield k -> (v + that(k)))
  }

  def -(that: TokenVec): TokenVec = {
    require(map.keySet == that.keySet)
    TokenVec(for ((k, v) <- map) yield k -> (v - that(k)))
  }

  def *(coeff: Double): TokenVec = TokenVec(map.map { case (token, value) => token -> value * coeff })
  def /(denom: Double): TokenVec = TokenVec(map.map { case (token, value) =>
    val vd = value / denom
    token -> (if (!vd.isNaN) vd else 0)
  })

  def abs: Double = map.values.map(v => v * v).sum
  def unit: TokenVec = this / abs

  def reorient(p: Program): Program = {
    val map2 = map.map { case (token, v) => token -> Value(v, token) }
    Program(p.name, p.rules.map(r => Rule(r.name, Value(r.coeff.prov, map2), r.head, r.body)))
  }

  def limitUpper(v: Double): TokenVec = TokenVec(map.mapValues(x => Math.min(v, x)))
  def limitLower(v: Double): TokenVec = TokenVec(map.mapValues(x => Math.max(v, x)))
}

object TokenVec {
  def apply(tokens: Set[Token], random: Random): TokenVec = {
    TokenVec(tokens.map(t => t -> random.nextDouble()).toMap)
  }
  def zero(tokens: Set[Token]): TokenVec = TokenVec(tokens.map(t => t -> 0.0).toMap)
}
