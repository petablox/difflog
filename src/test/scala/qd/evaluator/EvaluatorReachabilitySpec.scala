package qd
package evaluator

import org.scalatest.FunSuite
import qd.graphs.{Graphs, Reachability}

class EvaluatorReachabilitySpec(evaluator: Evaluator) extends FunSuite {
  for (graph <- Graphs.Graphs; gp <- Reachability.Programs) {
    val program = gp(graph)
    test(s"Applying evaluator ${evaluator.getClass} to program ${program.name} and graph ${graph.name}") {
      val idb = evaluator(program, graph.edb)
      val produced = idb(Graphs.path)
      assert(produced.support.forall(_._1.length == 2))
      assert(produced.support.map(tv => (tv._1(0), tv._1(1))).toSet == graph.reachable)
    }
  }
}

class NaiveEvaluatorShuntSpec extends EvaluatorReachabilitySpec(NaiveEvaluator)
// class SeminaiveEvaluatorShuntSpec extends EvaluatorReachabilitySpec(SeminaiveEvaluatorShunt[FValue])
// class TrieEvaluatorShuntSpec extends EvaluatorReachabilitySpec(TrieEvaluatorShunt[FValue])
