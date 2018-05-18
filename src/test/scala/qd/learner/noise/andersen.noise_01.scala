package qd
package learner
import org.scalatest.{FunSuite, Ignore}

class Genandersen_1 extends Problem {
	override val name = "andersen"
	val HSet = Range(0, 8).map(i => Atom(i)).toSet
	val H = Domain("H", HSet)
	val load = Relation("load", H,H)
	val assgn = Relation("assgn", H,H)
	val addr = Relation("addr", H,H)
	val store = Relation("store", H,H)
	val pt = Relation("pt", H,H)
	val loadTuples = Set((7, 2)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val assgnTuples = Set((4, 1)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val ptTuples = Set((1, 2),(2, 3),(3, 5),(5, 6),(4, 2),(7, 5),(2, 6)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val addrTuples = Set((1, 2),(2, 3),(3, 5),(5, 6)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val storeTuples = Set((4, 5)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
override val edb = Config(
load -> (Instance(load) ++ loadTuples.map(t => t -> One).toMap),
assgn -> (Instance(assgn) ++ assgnTuples.map(t => t -> One).toMap),
addr -> (Instance(addr) ++ addrTuples.map(t => t -> One).toMap),
store -> (Instance(store) ++ storeTuples.map(t => t -> One).toMap),
)
override val refOut = Config(
pt -> (Instance(pt) ++ ptTuples.map(t => t -> One).toMap),
)
	val x2 = Variable("x2",H)
	val x3 = Variable("x3",H)
	val x0 = Variable("x0",H)
	val x1 = Variable("x1",H)
	val soup = Set(
		Rule(0, Value(0.5, Token(0)), pt(x0,x1),addr(x0,x1)),
		Rule(1, Value(0.5, Token(1)), pt(x0,x1),assgn(x0,x1)),
		Rule(2, Value(0.5, Token(2)), pt(x0,x1),load(x0,x1)),
		Rule(3, Value(0.5, Token(3)), pt(x0,x1),store(x0,x1)),
		Rule(4, Value(0.5, Token(4)), pt(x2,x1),pt(x0,x1),pt(x2,x0)),
		Rule(5, Value(0.5, Token(5)), pt(x2,x1),addr(x2,x0),pt(x0,x1)),
		Rule(6, Value(0.5, Token(6)), pt(x2,x1),assgn(x2,x0),pt(x0,x1)),
		Rule(7, Value(0.5, Token(7)), pt(x2,x1),load(x2,x0),pt(x0,x1)),
		Rule(8, Value(0.5, Token(8)), pt(x2,x1),pt(x0,x1),store(x2,x0)),
		Rule(9, Value(0.5, Token(9)), pt(x2,x1),addr(x0,x1),pt(x2,x0)),
		Rule(10, Value(0.5, Token(10)), pt(x2,x1),assgn(x0,x1),pt(x2,x0)),
		Rule(11, Value(0.5, Token(11)), pt(x2,x1),load(x0,x1),pt(x2,x0)),
		Rule(12, Value(0.5, Token(12)), pt(x2,x1),pt(x2,x0),store(x0,x1)),
		Rule(13, Value(0.5, Token(13)), pt(x3,x1),pt(x0,x1),pt(x2,x0),pt(x3,x2)),
		Rule(14, Value(0.5, Token(14)), pt(x3,x1),addr(x3,x2),pt(x0,x1),pt(x2,x0)),
		Rule(15, Value(0.5, Token(15)), pt(x3,x1),assgn(x3,x2),pt(x0,x1),pt(x2,x0)),
		Rule(16, Value(0.5, Token(16)), pt(x3,x1),load(x3,x2),pt(x0,x1),pt(x2,x0)),
		Rule(17, Value(0.5, Token(17)), pt(x3,x1),pt(x0,x1),pt(x2,x0),store(x3,x2)),
		Rule(18, Value(0.5, Token(18)), pt(x3,x1),addr(x2,x0),pt(x0,x1),pt(x3,x2)),
		Rule(19, Value(0.5, Token(19)), pt(x3,x1),assgn(x2,x0),pt(x0,x1),pt(x3,x2)),
		Rule(20, Value(0.5, Token(20)), pt(x3,x1),load(x2,x0),pt(x0,x1),pt(x3,x2)),
		Rule(21, Value(0.5, Token(21)), pt(x3,x1),pt(x0,x1),pt(x3,x2),store(x2,x0)),
		Rule(22, Value(0.5, Token(22)), pt(x3,x1),store(x2,x0),pt(x0,x1),pt(x2,x3)),
		Rule(23, Value(0.5, Token(23)), pt(x3,x1),addr(x0,x1),pt(x2,x0),pt(x3,x2)),
		Rule(24, Value(0.5, Token(24)), pt(x3,x1),assgn(x0,x1),pt(x2,x0),pt(x3,x2)),
		Rule(25, Value(0.5, Token(25)), pt(x3,x1),load(x0,x1),pt(x2,x0),pt(x3,x2)),
		Rule(26, Value(0.5, Token(26)), pt(x3,x1),pt(x2,x0),pt(x3,x2),store(x0,x1)),
	)
	val soupProg = Program("andersenSoup", soup)
	val evaluator = SeminaiveEvaluator(soupProg)

	override val expected: Set[Any] = Set(1, 7, 17, 23)
	override val maxVarCount: Int = 20
}
