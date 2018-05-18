package qd
package learner
import org.scalatest.{FunSuite, Ignore}

class TestSamegenNLP extends Problem {
  val name = "samegen"
  val VSet = Range(0, 10).map(i => Atom(i)).toSet
  val V = Domain("V", VSet)
  val parent = Relation("parent", V,V)
  val sgen = Relation("sgen", V,V)
  val parentTuples = Set((2, 1),(3, 1),(4, 2),(5, 2),(6, 3),(7, 3),(8, 9)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
  val sgenTuples = Set((2, 2),(3, 3),(4, 4),(5, 5),(6, 6),(7, 7),(2, 3),(3, 2),(4, 5),(4, 6),(4, 7),(5, 4),(5, 6),(5, 7),(6, 7),(6, 4),(6, 5),(6, 7),(7, 4),(7, 5),(7, 6),(8, 8)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
  val edb = Config(
    parent -> (Instance(parent) ++ parentTuples.map(t => t -> One).toMap),
    )
  val refOut = Config(
    sgen -> (Instance(sgen) ++ sgenTuples.map(t => t -> One).toMap),
    )
  val x1V = Variable("x1V",V)
  val x3V = Variable("x3V",V)
  val x0V = Variable("x0V",V)
  val x2V = Variable("x2V",V)

  // result/nlp/samegen_6012.nlp
  val soup: Set[Rule] = Set(
    Rule(1, Value(0.497, Token(1)), sgen(x1V, x0V),parent(x1V, x0V)),
    Rule(2, Value(0.350, Token(2)), sgen(x1V, x0V),parent(x1V, x0V)),
    Rule(3, Value(0.178, Token(3)), sgen(x1V, x0V),sgen(x1V, x0V)),
    Rule(4, Value(0.173, Token(4)), sgen(x2V, x0V),parent(x1V, x0V), parent(x2V, x1V)),
    Rule(5, Value(0.062, Token(5)), sgen(x2V, x0V),sgen(x1V, x0V), parent(x2V, x1V)),
    Rule(6, Value(0.044, Token(6)), sgen(x1V, x0V),sgen(x1V, x0V)),
  )

  val soupProg: Program = Program("EscapeSoup", soup)
  val expected = Set(28, 133)
  val maxVarCount: Int = 20
}
