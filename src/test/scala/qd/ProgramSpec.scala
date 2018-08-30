package qd

import org.scalatest.FunSuite
import graphs.Graphs
import qd.evaluator.{SeminaiveEvaluatorShunt, TrieEvaluatorShunt}

class ProgramSpec extends FunSuite {

  val edge: Relation = Graphs.edge
  val path: Relation = Graphs.path
  val scc: Relation = Graphs.scc

  val p = Program.skeleton[FValue]("P", Set(edge), Set(path), Set(scc), 3, 4)
  // val p = Program.skeleton[FValue]("P", Set(edge), Set(), Set(path), 3, 4)
  // val p = Program.skeleton[FValue]("P", Set(edge), Set(path), Set(scc), 2, 4)
  /* for ((_, rules) <- p.rules.groupBy(_.head.relation);
       rule <- rules) {
    println(rule)
    // println(s"$rule NumLiterals${rule.body.size}.")
  } */
  // p.rules.take(5).foreach(println)
  // val evaluator = SeminaiveEvaluator(p)
  val evaluator = TrieEvaluatorShunt(p)

  for (graph <- Graphs.Graphs.take(1)) {
    test(s"Applying trie evaluator to big program and graph ${graph.name}") {
      println(s"Applying trie evaluator to big program (${p.rules.size}) and graph ${graph.name}...")
      val idb = evaluator(graph.edb)
      val produced = idb(Graphs.path)
      println(s"Applying seminaive evaluator to big program and graph ${graph.name}. Done!")
      assert(produced.support.forall(_._1.length == 2))
      assert(produced.support.map(tv => (tv._1(0), tv._1(1))).toSet == graph.reachable)
    }
  }

}
