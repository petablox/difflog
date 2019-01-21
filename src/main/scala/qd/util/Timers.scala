package qd
package util

import scala.collection.mutable

object Timers {

  private val timers: mutable.Map[Any, Long] = mutable.Map()

  def apply[T](name: Any)(thunk: => T): T = {
    val startTime = System.nanoTime()
    val ans: T = thunk
    val endTime = System.nanoTime()
    timers.synchronized {
      if (!timers.contains(name)) timers.put(name, 0l)
      val totalTime = timers(name) + (endTime - startTime)
      timers += name -> totalTime
    }
    ans
  }

  def getSnapshot: Map[Any, Long] = timers.toMap
  def reset(): Unit = timers.clear()

}
