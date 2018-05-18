package qd
package learner

import scala.util.Random

case class TokenVec(m: Map[Token, Double]) extends Map[Token, Double] {
  override def +[V >: Double](kv: (Token, V)): Map[Token, V] = m + kv
  override def get(key: Token): Option[Double] = m.get(key)
  override def iterator: Iterator[(Token, Double)] = m.iterator
  override def -(key: Token): TokenVec = TokenVec(m - key)

  def +(that: TokenVec): TokenVec = {
    require(m.keySet == that.keySet)
    TokenVec(for ((k, v) <- m) yield k -> (v + that(k)))
  }

  def -(that: TokenVec): TokenVec = {
    require(m.keySet == that.keySet)
    TokenVec(for ((k, v) <- m) yield k -> (v - that(k)))
  }

  def *(coeff: Double): TokenVec = TokenVec(m.map { case (token, value) => token -> value * coeff })
  def /(denom: Double): TokenVec = TokenVec(m.map { case (token, value) =>
    val vd = value / denom
    token -> (if (!vd.isNaN) vd else 0)
  })

  def abs: Double = Math.sqrt(m.values.map(v => v * v).sum)
  def unit: TokenVec = this / abs

  def reorient(p: Program): Program = {
    val map2 = m.map { case (token, v) => token -> Value(v, token) }
    Program(p.name, p.rules.map(r => Rule(r.name, Value(r.coeff.prov, map2), r.head, r.body)))
  }

  def limitUpper(v: Double): TokenVec = TokenVec(m.mapValues(x => Math.min(v, x)))

  def limitUpper(maxV: Double, pos: TokenVec): TokenVec = {
    val mp = m.map { case (token, oldV) => token -> (if (maxV >= oldV || pos(token) > maxV) oldV else maxV) }
    TokenVec(mp)
  }

  def limitLower(v: Double): TokenVec = TokenVec(m.mapValues(x => Math.max(v, x)))

  def limitLower(minV: Double, pos: TokenVec): TokenVec = {
    val mp = m.map { case (token, oldV) => token -> (if (minV <= oldV || pos(token) < minV) oldV else minV) }
    TokenVec(mp)
  }

}

object TokenVec {
  def apply(tokens: Set[Token], random: Random): TokenVec = {
    TokenVec(tokens.map(t => t -> random.nextDouble()).toMap)
  }
  def zero(tokens: Set[Token]): TokenVec = TokenVec(tokens.map(t => t -> 0.0).toMap)
}
