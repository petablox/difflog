package qd
package learner
import org.scalatest.{FunSuite, Ignore}

class TestAncestorNLP extends Problem {
  val name = "ancestor"
  val PSet = Range(0, 5).map(i => Atom(i)).toSet
  val P = Domain("P", PSet)
  val father = Relation("father", P,P)
  val mother = Relation("mother", P,P)
  val parent = Relation("parent", P,P)
  val ancestor = Relation("ancestor", P,P)
  val ancestorTuples = Set((1, 0),(4, 3),(2, 1),(3, 2),(2, 0),(3, 1),(3, 0),(4, 2),(4, 1),(4, 0)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
  val fatherTuples = Set((1, 0),(4, 3)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
  val parentTuples = Set((1, 0),(4, 3),(2, 1),(3, 2)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
  val motherTuples = Set((2, 1),(3, 2)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
  val edb = Config(
    father -> (Instance[FValue](father) ++ fatherTuples.map(t => t -> FValue.One).toMap),
    mother -> (Instance[FValue](mother) ++ motherTuples.map(t => t -> FValue.One).toMap),
    )
  val refOut = Config(
    parent -> (Instance[FValue](parent) ++ parentTuples.map(t => t -> FValue.One).toMap),
    ancestor -> (Instance[FValue](ancestor) ++ ancestorTuples.map(t => t -> FValue.One).toMap),
    )
  val x2P = Variable("x2P",P)
  val x0P = Variable("x0P",P)
  val x1P = Variable("x1P",P)

  // result/nlp/ancestor_3519.nlp
  val soup : Set[Rule[FValue]] = Set(
    Rule(1, FValue(0.333, Token(1)), ancestor(x1P, x0P),mother(x1P, x0P)),
    Rule(2, FValue(0.296, Token(2)), ancestor(x1P, x0P),father(x1P, x0P)),
    Rule(3, FValue(0.223, Token(3)), parent(x1P, x0P),father(x1P, x0P)),
    Rule(4, FValue(0.228, Token(4)), ancestor(x1P, x0P),mother(x1P, x0P)),
    Rule(5, FValue(0.214, Token(5)), parent(x1P, x0P),mother(x1P, x0P)),
    Rule(6, FValue(0.210, Token(6)), ancestor(x1P, x0P),father(x1P, x0P)),
    Rule(7, FValue(0.160, Token(7)), parent(x1P, x0P),father(x1P, x0P)),
    Rule(8, FValue(0.144, Token(8)), parent(x1P, x0P),mother(x1P, x0P)),
    Rule(9, FValue(0.124, Token(9)), parent(x1P, x0P),parent(x1P, x0P)),
    Rule(10, FValue(0.118, Token(10)), parent(x1P, x0P),ancestor(x1P, x0P)),
    Rule(11, FValue(0.122, Token(11)), ancestor(x1P, x0P),parent(x1P, x0P)),
    Rule(12, FValue(0.102, Token(12)), ancestor(x1P, x0P),ancestor(x1P, x0P)),
    Rule(13, FValue(0.076, Token(13)), ancestor(x2P, x0P),mother(x1P, x0P), mother(x2P, x1P)),
    Rule(14, FValue(0.070, Token(14)), ancestor(x2P, x0P),mother(x1P, x0P), father(x2P, x1P)),
    Rule(15, FValue(0.068, Token(15)), ancestor(x2P, x0P),father(x1P, x0P), mother(x2P, x1P)),
    Rule(16, FValue(0.063, Token(16)), ancestor(x2P, x0P),father(x1P, x0P), father(x2P, x1P)),
    Rule(17, FValue(0.049, Token(17)), parent(x1P, x0P),parent(x1P, x0P)),
    Rule(18, FValue(0.045, Token(18)), parent(x1P, x0P),ancestor(x1P, x0P)),
    Rule(19, FValue(0.036, Token(19)), parent(x2P, x0P),father(x1P, x0P), father(x2P, x1P)),
    Rule(20, FValue(0.035, Token(20)), parent(x2P, x0P),mother(x1P, x0P), father(x2P, x1P)),
    Rule(21, FValue(0.033, Token(21)), ancestor(x1P, x0P),parent(x1P, x0P)),
    Rule(22, FValue(0.032, Token(22)), parent(x2P, x0P),father(x1P, x0P), mother(x2P, x1P)),
  )
  val expected = Set(1,2,19,23)
  val maxVarCount: Int = 20
}
