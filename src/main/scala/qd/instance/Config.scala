package qd
package instance

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Configurations

case class Config[T <: Value[T]](map: Map[Relation, Instance[T]])(implicit vs: Semiring[T])
extends (Relation => Instance[T]) {
  require(map.forall { case (relation, instance) => relation.signature == instance.signature })

  override def apply(relation: Relation): Instance[T] = map.getOrElse(relation, Instance(relation))
  def +(ri: (Relation, Instance[T])): Config[T] = {
    val (relation, instance) = ri
    val newInstance = this(relation) ++ instance
    Config(map + (relation -> newInstance))
  }

  def add(relation: Relation, tuple: DTuple, value: T): Config[T] = {
    val newInstance = this(relation) + (tuple -> value)
    Config(map + (relation -> newInstance))
  }

  def nonEmptySupport: Boolean = map.values.exists(_.nonEmpty)
}

object Config {
  def apply[T <: Value[T]](firstPair: (Relation, Instance[T]), remainingPairs: (Relation, Instance[T])*)
                          (implicit vs: Semiring[T]): Config[T] = {
    Config((firstPair +: remainingPairs).toMap)
  }
  def apply[T <: Value[T]]()(implicit vs: Semiring[T]): Config[T] = Config[T](Map[Relation, Instance[T]]())
}
