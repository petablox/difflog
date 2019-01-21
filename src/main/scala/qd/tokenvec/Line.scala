package qd
package tokenvec

import qd.util.Contract

case class Line(points: IndexedSeq[TokenVec]) extends (Token => VecValue[FValue]) {

  Contract.require(points.nonEmpty)
  val tokens: Set[Token] = points.head.keySet
  Contract.require(points.map(_.keySet).forall(_ == tokens))

  override def apply(token: Token): VecValue[FValue] = VecValue(points.map(_(token)))

}
