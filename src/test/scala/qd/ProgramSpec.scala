package qd

import org.scalatest.FunSuite
import data.graphs.Graphs
import evaluator.{Evaluator, TrieSemiEvaluator}
import qd.Semiring.FValueSemiringObj
import qd.data.graphs.Graphs.Graph

import scala.util.Random

class ProgramSpec extends FunSuite {

  val edge: Relation = Graphs.edge
  val path: Relation = Graphs.path
  val scc: Relation = Graphs.scc

  val rng: Random = new Random(0)
  var ruleIndex = 0
  val weight: (Literal, Set[Literal]) => FValue = { (_, _) =>
    val ans = FValue(rng.nextDouble(), Token(s"R$ruleIndex"))
    // val ans = FValueSemiringObj.One
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
  } */
  // println(p.rules.size)

  val graph: Graph = Graphs.circle(5)
  val evaluator: Evaluator = TrieSemiEvaluator
  test(testName = s"Applying evaluator ${evaluator.getClass} to " +
    s"program ${p.name} of size ${p.rules.size} and " +
    s"graph ${graph.name}") {
    // val idb = SeminaiveEvaluator(p, graph.edb)
    val idb = evaluator(p, graph.edb)
    val produced = idb(Graphs.path)
    println(s"Applying seminaive evaluator to big program and graph ${graph.name}. Done!")
    assert(produced.support.forall(_._1.length == 2))
    assert(produced.support.map(tv => (tv._1(0), tv._1(1))).toSet == graph.reachable)
  }

}
