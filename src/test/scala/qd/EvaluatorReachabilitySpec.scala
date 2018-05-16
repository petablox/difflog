package qd

import org.scalatest.FunSuite

class EvaluatorReachabilitySpec(evalCtr: Program => Evaluator) extends FunSuite {
  for (graph <- Graphs.Graphs; gp <- Reachability.Programs) {
    val program = gp(graph)
    val evaluator = evalCtr(program)
    test(s"Applying evaluator ${evaluator.name} to program ${program.name} and graph ${graph.name}") {
      // val startTime = System.nanoTime()
      val idb = evaluator(graph.edb)
      // val endTime = System.nanoTime()
      val produced = idb(graph.path)
      assert(produced.support.forall(_._1.length == 2))
      assert(produced.support.map(tv => (tv._1(0), tv._1(1))).toSet == graph.reachable)
      // println(s"TIME! ${graph.name}, ${program.name}, ${evaluator.name}: ${(endTime - startTime) / 1.0e9}")
    }
  }
}

/* class NaiveEvaluatorSpec extends EvaluatorReachabilitySpec(NaiveEvaluator)
class SeminaiveEvaluatorSpec extends EvaluatorReachabilitySpec(SeminaiveEvaluator) */