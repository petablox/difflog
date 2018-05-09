package qd

import scala.util.hashing.MurmurHash3

sealed abstract class RevList[+A](val length: Int) {
  // Fast hash code implementation
  // Every instance [[l]] of this class has the same hash code as [[l.toSeq]]
  // Consult: https://raw.githubusercontent.com/scala/scala/v2.12.0/src/library/scala/collection/GenSeqLike.scala
  // Consult: https://raw.githubusercontent.com/scala/scala/v2.12.2/src/library/scala/util/hashing/MurmurHash3.scala
  final val hashSeed = MurmurHash3.seqSeed
  def preHash: Int
  override def hashCode(): Int

  def toSeq: Seq[A] = this match {
    case RevEmpty => Seq()
    case RevSnoc(xs, x) => xs.toSeq :+ x
  }
}

case object RevEmpty extends RevList[Nothing](0) {
  override val preHash: Int = hashSeed
  override val hashCode: Int = MurmurHash3.finalizeHash(preHash, length)
}

case class RevSnoc[+A](xs: RevList[A], x: A) extends RevList[A](xs.length + 1) {
  override val preHash: Int = MurmurHash3.mix(xs.preHash, x.##)
  override def hashCode(): Int = MurmurHash3.finalizeHash(preHash, length)
}
