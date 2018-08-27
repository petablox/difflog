package qd
package learner
import org.scalatest.{FunSuite, Ignore}

class Genescape_10 extends Problem {
	override val name = "escape"
	val HSet = Range(0, 5).map(i => Atom(i)).toSet
	val H = Domain("H", HSet)
	val MSet = Range(0, 3).map(i => Atom(i)).toSet
	val M = Domain("M", MSet)
	val VSet = Range(0, 4).map(i => Atom(i)).toSet
	val V = Domain("V", VSet)
	val VH = Relation("VH", V,H)
	val HFH = Relation("HFH", H,H)
	val MmethRet = Relation("MmethRet", M,V)
	val MmethArg = Relation("MmethArg", M,V)
	val rHH = Relation("rHH", H,H)
	val rMH = Relation("rMH", M,H)
	val rRH = Relation("rRH", M,H)
	val MmethArgTuples = Set((0, 0),(1, 3),(2, 2)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val rHHTuples = Set((1, 2),(2, 3),(0, 2),(1, 3),(0, 3),(1, 1),(1, 4)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val VHTuples = Set((1, 1),(0, 0),(2, 2),(3, 3)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val rRHTuples = Set((0, 1),(0, 2),(0, 3),(2, 1),(2, 2),(2, 3),(1, 0)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val MmethRetTuples = Set((0, 1),(2, 1)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val HFHTuples = Set((0, 1),(1, 2),(2, 3),(0, 2)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val rMHTuples = Set((0, 0),(0, 1),(0, 2),(0, 3),(1, 3),(2, 2),(2, 3),(1, 1),(2, 1)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
override val edb = Config(
VH -> (Instance[FValue](VH) ++ VHTuples.map(t => t -> FValue.One).toMap),
HFH -> (Instance[FValue](HFH) ++ HFHTuples.map(t => t -> FValue.One).toMap),
MmethRet -> (Instance[FValue](MmethRet) ++ MmethRetTuples.map(t => t -> FValue.One).toMap),
MmethArg -> (Instance[FValue](MmethArg) ++ MmethArgTuples.map(t => t -> FValue.One).toMap),
)
override val refOut = Config(
rHH -> (Instance[FValue](rHH) ++ rHHTuples.map(t => t -> FValue.One).toMap),
rMH -> (Instance[FValue](rMH) ++ rMHTuples.map(t => t -> FValue.One).toMap),
rRH -> (Instance[FValue](rRH) ++ rRHTuples.map(t => t -> FValue.One).toMap),
)
	val x2 = Variable("x2",H)
	val x3 = Variable("x3",H)
	val x0 = Variable("x0",H)
	val x1 = Variable("x1",H)
	val soup = Set(
		Rule(0, FValue(0.5, Token(0)), rHH(x0,x2),HFH(x0,x2)),
		Rule(1, FValue(0.5, Token(1)), rHH(x3,x2),HFH(x0,x2),rHH(x3,x0)),
		Rule(2, FValue(0.5, Token(2)), rHH(x0,x3),HFH(x0,x2),rHH(x2,x3)),
		Rule(3, FValue(0.5, Token(3)), rHH(x2,x1),rHH(x0,x1),rHH(x2,x0)),
		Rule(4, FValue(0.5, Token(4)), rMH(x0,x1),rRH(x0,x1)),
		Rule(5, FValue(0.5, Token(5)), rMH(x2,x1),MmethArg(x2,x0),VH(x0,x1)),
		Rule(6, FValue(0.5, Token(6)), rMH(x2,x1),MmethRet(x2,x0),VH(x0,x1)),
		Rule(7, FValue(0.5, Token(7)), rMH(x2,x1),rMH(x0,x1),rRH(x2,x1)),
		Rule(8, FValue(0.5, Token(8)), rMH(x2,x1),rHH(x0,x1),rRH(x2,x1)),
		Rule(9, FValue(0.5, Token(9)), rMH(x2,x1),VH(x0,x1),rRH(x2,x1)),
		Rule(10, FValue(0.5, Token(10)), rMH(x3,x2),HFH(x0,x2),rRH(x3,x0)),
		Rule(11, FValue(0.5, Token(11)), rMH(x3,x2),HFH(x0,x2),rMH(x3,x0)),
		Rule(12, FValue(0.5, Token(12)), rMH(x0,x1),rHH(x1,x2),rRH(x0,x1)),
		Rule(13, FValue(0.5, Token(13)), rMH(x0,x2),rHH(x1,x2),rRH(x0,x1)),
		Rule(14, FValue(0.5, Token(14)), rMH(x0,x2),rHH(x1,x2),rMH(x0,x1)),
		Rule(15, FValue(0.5, Token(15)), rRH(x0,x1),rMH(x0,x1)),
		Rule(16, FValue(0.5, Token(16)), rRH(x2,x1),MmethArg(x2,x0),VH(x0,x1)),
		Rule(17, FValue(0.5, Token(17)), rRH(x2,x1),MmethRet(x2,x0),VH(x0,x1)),
		Rule(18, FValue(0.5, Token(18)), rRH(x2,x1),rMH(x2,x1),rRH(x0,x1)),
		Rule(19, FValue(0.5, Token(19)), rRH(x2,x1),rHH(x0,x1),rMH(x2,x1)),
		Rule(20, FValue(0.5, Token(20)), rRH(x2,x1),VH(x0,x1),rMH(x2,x1)),
		Rule(21, FValue(0.5, Token(21)), rRH(x3,x2),HFH(x0,x2),rRH(x3,x0)),
		Rule(22, FValue(0.5, Token(22)), rRH(x3,x2),HFH(x0,x2),rMH(x3,x0)),
		Rule(23, FValue(0.5, Token(23)), rRH(x0,x1),rHH(x1,x2),rMH(x0,x1)),
		Rule(24, FValue(0.5, Token(24)), rRH(x0,x2),rHH(x1,x2),rRH(x0,x1)),
		Rule(25, FValue(0.5, Token(25)), rRH(x0,x2),rHH(x1,x2),rMH(x0,x1)),
	)
	val soupProg = Program("escapeSoup", soup)
	val evaluator = SeminaiveEvaluator(soupProg)
	override val expected: Set[Any] = Set(1, 4, 6, 14, 18, 22)
	override val maxVarCount: Int = 20
}
