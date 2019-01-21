package qd
package evaluator

import org.scalatest.FunSuite
import Semiring.FValueSemiringObj
import data.graphs.{Graphs, Reachability}
import util.Timers

class StressEvaluatorSpec extends FunSuite {
  val evaluator: Evaluator = TrieJoinEvaluator
  val (programName, rules): (String, Set[Rule]) = Reachability.PP
  val graph: Graphs.Graph = Graphs.erdosRenyi(50, 0.1, 0)

  for (evaluator <- Set(TrieJoinEvaluator);
       (programName, rules) <- Set(Reachability.PP);
       graph <- Set(Graphs.erdosRenyi(50, 0.1, 0));
       i <- Range(0, 5)) {
    test(s"Applying evaluator $evaluator to program $programName and graph ${graph.name} (Iteration $i)") {
      val startTime = System.nanoTime()
      val idb = evaluator(rules, _ => FValueSemiringObj.One, graph.edb)
      val endTime = System.nanoTime()
      val produced = idb(Graphs.path)
      assert(produced.support.forall(_._1.length == 2))
      assert(produced.support.map(tv => (tv._1(0), tv._1(1))).toSet == graph.reachable)
      assert(produced.support.forall(_._2.v >= 1.0))

      val totalTime = endTime - startTime
      val fractions = Timers.getSnapshot.map({ case (name, time) => name -> (time.toDouble / totalTime) })
                                        .map({ case (name, frac) => s"$name: $frac" })
                                        .mkString("[", ", ", "]")
      Timers.reset()

      println(s"Iteration $i $fractions")
    }
  }
}
