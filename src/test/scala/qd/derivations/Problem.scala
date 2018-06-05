package qd
package derivations

import org.scalatest.FunSuite

abstract class Problem extends FunSuite {
  def name : Any
  def edb : Config[DValue]
  def soup : Set[Rule[DValue]]
  def maxVarCount : Int

  def p0: Program[DValue] = Program(s"Soup-$name", soup.filter(_.freeVariables.size <= maxVarCount))

  test(s"Applying seminaive evaluator to initial program of $name") {
    val startTime = System.nanoTime()
    val evaluator = SeminaiveEvaluator(p0)
    val out = evaluator(edb)
    var num_clauses = 1
    for (x <- out.iterator ) {
      println(x._1)
      for (y <- x._2.support) {
        num_clauses += y._2.r.size
        println(s"Value for tuple ${(y._1)}: ${(y._2)}")
      }
    }
    println(s"Total number of clauses: ${num_clauses}")
    val endTime = System.nanoTime()
    println(s"Evaluation finished in ${(endTime - startTime) / 1.0e9} seconds.")
  }

}
