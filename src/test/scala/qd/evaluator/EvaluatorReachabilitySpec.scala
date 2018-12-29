package qd
package evaluator

import org.scalatest.FunSuite
import data.graphs.{Graphs, Reachability}
import qd.Semiring.FValueSemiringObj

class EvaluatorReachabilitySpec(evaluator: Evaluator) extends FunSuite {
  for ((programName, rules) <- Reachability.Programs; graph <- Graphs.Graphs) {
    test(s"Applying evaluator $evaluator to program $programName and graph ${graph.name}") {
      val idb = evaluator(rules, _ => FValueSemiringObj.One, graph.edb)
      val produced = idb(Graphs.path)
      assert(produced.support.forall(_._1.length == 2))
      assert(produced.support.map(tv => (tv._1(0), tv._1(1))) == graph.reachable)
      assert(produced.support.forall(_._2.v >= 1.0))
    }
  }
}

class NaiveEvaluatorSpec extends EvaluatorReachabilitySpec(NaiveEvaluator)
class SeminaiveEvaluatorSpec extends EvaluatorReachabilitySpec(SeminaiveEvaluator)
class TrieEvaluatorSpec extends EvaluatorReachabilitySpec(TrieEvaluator)
class TrieSemiEvaluatorSpec extends EvaluatorReachabilitySpec(TrieSemiEvaluator)
