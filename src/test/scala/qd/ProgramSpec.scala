package qd

import org.scalatest.FunSuite
import data.graphs.Graphs
import evaluator.TrieEvaluator

import scala.util.Random

class ProgramSpec extends FunSuite {

  val edge: Relation = Graphs.edge
  val path: Relation = Graphs.path
  val scc: Relation = Graphs.scc

  val rng: Random = new Random(0)
  var ruleIndex = 0
  val weight: (Literal, Set[Literal]) => FValue = { (_, _) =>
    val ans = FValue(rng.nextDouble(), Token(s"R$ruleIndex"))
    ruleIndex = ruleIndex + 1
    ans
  }

  val maxLiterals = 3
  val maxVars = 4

  val p: Program[FValue] = Program.skeleton[FValue]("P", Set(edge), Set(path), Set(scc),
                                                    weight, maxLiterals, maxVars)

  if (maxLiterals == 3 && maxVars == 4) {
    test("Since maxLiterals == 3 and maxVars == 4, the skeleton program should have 21443 rules") {
      assert(p.rules.size == 21443)
    }
  }

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
