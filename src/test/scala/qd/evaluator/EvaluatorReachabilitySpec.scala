package qd
package evaluator

import org.scalatest.FunSuite
import qd.graphs.{Graphs, Reachability}

class EvaluatorReachabilitySpec(evalCtr: Program[FValue] => Evaluator[FValue]) extends FunSuite {
  for (graph <- Graphs.Graphs; gp <- Reachability.Programs) {
    val program = gp(graph)
    val evaluator = evalCtr(program)
    test(s"Applying evaluator ${evaluator.name} to program ${program.name} and graph ${graph.name}") {
      val idb = evaluator(graph.edb)
      val produced = idb(Graphs.path)
      assert(produced.support.forall(_._1.length == 2))
      assert(produced.support.map(tv => (tv._1(0), tv._1(1))).toSet == graph.reachable)
    }
  }
}

class NaiveEvaluatorSpec extends EvaluatorReachabilitySpec(NaiveEvaluator[FValue])
class SeminaiveEvaluatorSpec extends EvaluatorReachabilitySpec(SeminaiveEvaluator[FValue])