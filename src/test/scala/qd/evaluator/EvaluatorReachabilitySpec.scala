package qd
package evaluator

import org.scalatest.FunSuite
import data.graphs.{Graphs, Reachability}

class EvaluatorReachabilitySpec(evaluator: Evaluator) extends FunSuite {
  for (program <- Reachability.Programs; graph <- Graphs.Graphs) {
    test(s"Applying evaluator ${evaluator.getClass} to program ${program.name} and graph ${graph.name}") {
      val idb = evaluator(program, graph.edb)
      val produced = idb(Graphs.path)
      assert(produced.support.forall(_._1.length == 2))
      assert(produced.support.map(tv => (tv._1(0), tv._1(1))) == graph.reachable)
    }
  }
}

class NaiveEvaluatorSpec extends EvaluatorReachabilitySpec(NaiveEvaluator)
class SeminaiveEvaluatorSpec extends EvaluatorReachabilitySpec(SeminaiveEvaluator)
class TrieEvaluatorSpec extends EvaluatorReachabilitySpec(TrieEvaluator)
