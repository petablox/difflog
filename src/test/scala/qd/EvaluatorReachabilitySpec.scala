package qd

import org.scalatest.{FunSuite, Ignore}

class EvaluatorReachabilitySpec(evalCtr: Program => Evaluator) extends FunSuite {
  for (graph <- Graphs.Graphs; gp <- Reachability.Programs) {
    val program = gp(graph)
    val evaluator = evalCtr(program)
    test(s"Applying evaluator ${evaluator.name} to program ${program.name} and graph ${graph.name}") {
      // val startTime = System.nanoTime()
      val idb = evaluator(graph.edb)
      // val endTime = System.nanoTime()
      val produced = idb(graph.path)
      assert(produced.support.keys.forall(_.length == 2))
      assert(produced.support.keys.map(t => (t(0), t(1))) == graph.reachable)
      // println(s"TIME! ${graph.name}, ${program.name}, ${evaluator.name}: ${(endTime - startTime) / 1.0e9}")
    }
  }
}

@Ignore
class NaiveEvaluatorSpec extends EvaluatorReachabilitySpec(NaiveEvaluator)
@Ignore
class SeminaiveEvaluatorSpec extends EvaluatorReachabilitySpec(SeminaiveEvaluator)
