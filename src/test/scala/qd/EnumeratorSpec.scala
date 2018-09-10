package qd

import org.scalatest.FunSuite
import data.graphs.Graphs
import evaluator.{Evaluator, TrieSemiEvaluator}
import qd.data.graphs.Graphs.Graph

import scala.util.Random

class EnumeratorSpec extends FunSuite {

  val edge: Relation = Graphs.edge
  val path: Relation = Graphs.path
  val scc: Relation = Graphs.scc

  val rng: Random = new Random(0)
  var ruleIndex = 0

  def weight(l: Literal, ls: Seq[Literal]): (Token, FValue) = {
    val token = Token(s"R$ruleIndex")
    ruleIndex = ruleIndex + 1
    val value = rng.nextDouble()
    (token, FValue(value, token))
  }

  val maxLiterals = 3
  val maxVars = 4

  val posRules: (Map[Token, FValue], Set[Rule[FValue]]) = Enumerator.skeleton(Set(edge), Set(path), Set(scc),
                                                                              weight, maxLiterals, maxVars)

  if (maxLiterals == 3 && maxVars == 4) {
    test("Since maxLiterals == 3 and maxVars == 4, the skeleton program should have 21443 rules") {
      assert(posRules._2.size == 21443)
    }
  }

  val graph: Graph = Graphs.circle(5)
  val evaluator: Evaluator = TrieSemiEvaluator
  test(testName = s"Applying evaluator ${evaluator.getClass} to " +
    s"${posRules._2.size} rules and graph ${graph.name}") {
    // val idb = SeminaiveEvaluator(p, graph.edb)
    val idb = evaluator(posRules._2, graph.edb)
    val produced = idb(Graphs.path)
    assert(produced.support.forall(_._1.length == 2))
    assert(produced.support.map(tv => (tv._1.head, tv._1(1))) == graph.reachable)
  }

}
