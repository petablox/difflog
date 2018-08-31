package qd

import org.scalatest.FunSuite
import data.graphs.Graphs
import evaluator.TrieEvaluator

class ProgramSpec extends FunSuite {

  val edge: Relation = Graphs.edge
  val path: Relation = Graphs.path
  val scc: Relation = Graphs.scc

  val p: Program[FValue] = Program.skeleton[FValue]("P", Set(edge), Set(path), Set(scc), 3, 4)
  /* for ((_, rules) <- p.rules.groupBy(_.head.relation);
       rule <- rules) {
    println(rule)
    // println(s"$rule NumLiterals${rule.body.size}.")
  } */
  // p.rules.take(5).foreach(println)

  for (graph <- Graphs.Graphs.take(1)) {
    val evaluator = TrieEvaluator
    test(testName = s"Applying evaluator ${evaluator.getClass} to " +
                    s"program ${p.name} of size ${p.rules.size} and " +
                    s"graph ${graph.name}") {
      // val idb = SeminaiveEvaluator(p, graph.edb)
      val idb = TrieEvaluator(p, graph.edb)
      val produced = idb(Graphs.path)
      println(s"Applying seminaive evaluator to big program and graph ${graph.name}. Done!")
      assert(produced.support.forall(_._1.length == 2))
      assert(produced.support.map(tv => (tv._1(0), tv._1(1))).toSet == graph.reachable)
    }
  }

}
