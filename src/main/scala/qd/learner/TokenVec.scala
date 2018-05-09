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

  def *(coeff: Double): TokenVec = {
    require(0.0 <= coeff && coeff <= 1.0)
    TokenVec(map.mapValues(_ * coeff))
  }

  def /(denom: Double): TokenVec = {
    require(0.0 <= denom)
    TokenVec(map.mapValues(_ / denom))
  }

  def abs: Double = map.values.map(v => v * v).sum / map.size

  def reorient(p: Program): Program = {
    val map2 = map.map { case (token, v) => token -> Value(v, token) }
    Program(p.name, p.rules.map(r => Rule(r.name, Value(r.coeff.prov, map2), r.head, r.body)))
  }
}

object TokenVec {
  def apply(tokens: Set[Token], random: Random): TokenVec = {
    TokenVec(tokens.map(t => t -> random.nextDouble()).toMap)
  }
}
