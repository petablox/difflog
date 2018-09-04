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

  def weight(l: Literal, ls: Set[Literal]): (Token, FValue) = {
    val token = Token(s"R$ruleIndex")
    ruleIndex = ruleIndex + 1
    val value = rng.nextDouble()
    (token, FValue(value, token))
  }

  val maxLiterals = 3
  val maxVars = 4

  val posRules: (Map[Token, FValue], Set[Rule[FValue]]) = Program.skeleton(Set(edge), Set(path), Set(scc),
                                                                           weight, maxLiterals, maxVars)
  val program: Program[FValue] = Program("P", posRules._2)

  if (maxLiterals == 3 && maxVars == 4) {
    test("Since maxLiterals == 3 and maxVars == 4, the skeleton program should have 21443 rules") {
      assert(posRules._2.size == 21443)
    }
  }

  val graph: Graph = Graphs.circle(5)
  val evaluator: Evaluator = TrieSemiEvaluator
  test(testName = s"Applying evaluator ${evaluator.getClass} to " +
    s"program ${program.name} of size ${program.rules.size} and " +
    s"graph ${graph.name}") {
    // val idb = SeminaiveEvaluator(p, graph.edb)
    val idb = evaluator(program, graph.edb)
    val produced = idb(Graphs.path)
    assert(produced.support.forall(_._1.length == 2))
    assert(produced.support.map(tv => (tv._1(0), tv._1(1))).toSet == graph.reachable)
  }

}
