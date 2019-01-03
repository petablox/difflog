package qd.util

object Contract {
  // Factoring out calls to Predef.require(...) and Predef.assert(...) allows us to turn them off and on and confirm
  // performance penalties
  val CHECK_CONTRACTS: Boolean = true
  def require(predicate: => Boolean): Unit = if (CHECK_CONTRACTS) { Predef.require(predicate) }
  def require(predicate: => Boolean, message: => Any): Unit = if (CHECK_CONTRACTS) { Predef.require(predicate, message) }
  def assert(predicate: => Boolean): Unit = if (CHECK_CONTRACTS) { Predef.assert(predicate) }
  def assert(predicate: => Boolean, message: => Any): Unit = if (CHECK_CONTRACTS) { Predef.assert(predicate, message) }
}
