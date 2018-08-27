package qd
package learner
import org.scalatest.{FunSuite, Ignore}

class Genancestor_5 extends Problem {
	override val name = "ancestor"
	val PSet = Range(0, 5).map(i => Atom(i)).toSet
	val P = Domain("P", PSet)
	val father = Relation("father", P,P)
	val mother = Relation("mother", P,P)
	val ancestor = Relation("ancestor", P,P)
	val parent = Relation("parent", P,P)
	val ancestorTuples = Set((1, 0),(4, 3),(2, 1),(2, 0),(3, 1),(3, 0),(4, 2),(4, 1),(4, 0)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val fatherTuples = Set((1, 0),(4, 3)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val parentTuples = Set((1, 0),(4, 3),(2, 1),(3, 2),(4, 0)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val motherTuples = Set((2, 1),(3, 2)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
override val edb = Config(
father -> (Instance[FValue](father) ++ fatherTuples.map(t => t -> FValue.One).toMap),
mother -> (Instance[FValue](mother) ++ motherTuples.map(t => t -> FValue.One).toMap),
)
override val refOut = Config(
ancestor -> (Instance[FValue](ancestor) ++ ancestorTuples.map(t => t -> FValue.One).toMap),
parent -> (Instance[FValue](parent) ++ parentTuples.map(t => t -> FValue.One).toMap),
)
	val x2P = Variable("x2P",P)
	val x0P = Variable("x0P",P)
	val x1P = Variable("x1P",P)
	val soup = Set(
		Rule(0, FValue(0.5, Token(0)), parent(x0P,x1P),ancestor(x0P,x1P)),
		Rule(1, FValue(0.5, Token(1)), parent(x0P,x1P),father(x0P,x1P)),
		Rule(2, FValue(0.5, Token(2)), parent(x0P,x1P),mother(x0P,x1P)),
		Rule(3, FValue(0.5, Token(3)), parent(x2P,x1P),ancestor(x0P,x1P),ancestor(x2P,x0P)),
		Rule(4, FValue(0.5, Token(4)), parent(x2P,x1P),ancestor(x0P,x1P),parent(x2P,x0P)),
		Rule(5, FValue(0.5, Token(5)), parent(x2P,x1P),ancestor(x0P,x1P),father(x2P,x0P)),
		Rule(6, FValue(0.5, Token(6)), parent(x2P,x1P),ancestor(x0P,x1P),mother(x2P,x0P)),
		Rule(7, FValue(0.5, Token(7)), parent(x2P,x1P),ancestor(x2P,x0P),parent(x0P,x1P)),
		Rule(8, FValue(0.5, Token(8)), parent(x2P,x1P),parent(x0P,x1P),parent(x2P,x0P)),
		Rule(9, FValue(0.5, Token(9)), parent(x2P,x1P),father(x2P,x0P),parent(x0P,x1P)),
		Rule(10, FValue(0.5, Token(10)), parent(x2P,x1P),mother(x2P,x0P),parent(x0P,x1P)),
		Rule(11, FValue(0.5, Token(11)), parent(x2P,x1P),ancestor(x2P,x0P),father(x0P,x1P)),
		Rule(12, FValue(0.5, Token(12)), parent(x2P,x1P),father(x0P,x1P),parent(x2P,x0P)),
		Rule(13, FValue(0.5, Token(13)), parent(x2P,x1P),father(x0P,x1P),father(x2P,x0P)),
		Rule(14, FValue(0.5, Token(14)), parent(x2P,x1P),father(x0P,x1P),mother(x2P,x0P)),
		Rule(15, FValue(0.5, Token(15)), parent(x2P,x1P),ancestor(x2P,x0P),mother(x0P,x1P)),
		Rule(16, FValue(0.5, Token(16)), parent(x2P,x1P),mother(x0P,x1P),parent(x2P,x0P)),
		Rule(17, FValue(0.5, Token(17)), parent(x2P,x1P),father(x2P,x0P),mother(x0P,x1P)),
		Rule(18, FValue(0.5, Token(18)), parent(x2P,x1P),mother(x0P,x1P),mother(x2P,x0P)),
		Rule(19, FValue(0.5, Token(19)), ancestor(x0P,x1P),parent(x0P,x1P)),
		Rule(20, FValue(0.5, Token(20)), ancestor(x0P,x1P),father(x0P,x1P)),
		Rule(21, FValue(0.5, Token(21)), ancestor(x0P,x1P),mother(x0P,x1P)),
		Rule(22, FValue(0.5, Token(22)), ancestor(x2P,x1P),ancestor(x0P,x1P),ancestor(x2P,x0P)),
		Rule(23, FValue(0.5, Token(23)), ancestor(x2P,x1P),ancestor(x0P,x1P),parent(x2P,x0P)),
		Rule(24, FValue(0.5, Token(24)), ancestor(x2P,x1P),ancestor(x0P,x1P),father(x2P,x0P)),
		Rule(25, FValue(0.5, Token(25)), ancestor(x2P,x1P),ancestor(x0P,x1P),mother(x2P,x0P)),
		Rule(26, FValue(0.5, Token(26)), ancestor(x2P,x1P),ancestor(x2P,x0P),parent(x0P,x1P)),
		Rule(27, FValue(0.5, Token(27)), ancestor(x2P,x1P),parent(x0P,x1P),parent(x2P,x0P)),
		Rule(28, FValue(0.5, Token(28)), ancestor(x2P,x1P),father(x2P,x0P),parent(x0P,x1P)),
		Rule(29, FValue(0.5, Token(29)), ancestor(x2P,x1P),mother(x2P,x0P),parent(x0P,x1P)),
		Rule(30, FValue(0.5, Token(30)), ancestor(x2P,x1P),ancestor(x2P,x0P),father(x0P,x1P)),
		Rule(31, FValue(0.5, Token(31)), ancestor(x2P,x1P),father(x0P,x1P),parent(x2P,x0P)),
		Rule(32, FValue(0.5, Token(32)), ancestor(x2P,x1P),father(x0P,x1P),father(x2P,x0P)),
		Rule(33, FValue(0.5, Token(33)), ancestor(x2P,x1P),father(x0P,x1P),mother(x2P,x0P)),
		Rule(34, FValue(0.5, Token(34)), ancestor(x2P,x1P),ancestor(x2P,x0P),mother(x0P,x1P)),
		Rule(35, FValue(0.5, Token(35)), ancestor(x2P,x1P),mother(x0P,x1P),parent(x2P,x0P)),
		Rule(36, FValue(0.5, Token(36)), ancestor(x2P,x1P),father(x2P,x0P),mother(x0P,x1P)),
		Rule(37, FValue(0.5, Token(37)), ancestor(x2P,x1P),mother(x0P,x1P),mother(x2P,x0P)),
	)
	val soupProg = Program("ancestorSoup", soup)
	val evaluator = SeminaiveEvaluator(soupProg)

	override val expected = Set(1,2,19,23)
	override val maxVarCount: Int = 20
}
