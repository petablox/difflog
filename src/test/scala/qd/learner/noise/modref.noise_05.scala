package qd
package learner
import org.scalatest.{FunSuite, Ignore}

class Genmodref_5 extends Problem {
	override val name = "modref"
	val ISet = Range(0, 8).map(i => Atom(i)).toSet
	val I = Domain("I", ISet)
	val HSet = Range(0, 8).map(i => Atom(i)).toSet
	val H = Domain("H", HSet)
	val FSet = Range(0, 8).map(i => Atom(i)).toSet
	val F = Domain("F", FSet)
	val MSet = Range(0, 8).map(i => Atom(i)).toSet
	val M = Domain("M", MSet)
	val VSet = Range(0, 8).map(i => Atom(i)).toSet
	val V = Domain("V", VSet)
	val MputInstFldInst = Relation("MputInstFldInst", M,V,F,V)
	val MputStatFldInst = Relation("MputStatFldInst", M,F,V)
	val MgetInstFldInst = Relation("MgetInstFldInst", M,V,V,F)
	val MI = Relation("MI", M,I)
	val VH = Relation("VH", V,H)
	val IM = Relation("IM", I,M)
	val MgetStatFldInst = Relation("MgetStatFldInst", M,V,F)
	val modStatField = Relation("modStatField", M,F)
	val refStatField = Relation("refStatField", M,F)
	val modInstField = Relation("modInstField", M,H,F)
	val refInstField = Relation("refInstField", M,H,F)
	val rMM = Relation("rMM", M,M)
	val modInstFieldTuples = Set((0, 4, 3),(1, 4, 3),(2, 4, 3),(3, 4, 3),(4, 4, 3),(0, 0, 3),(0, 2, 4),(0, 3, 2),(0, 6, 6),(0, 6, 7),(1, 0, 2),(1, 3, 1),(1, 4, 7),(2, 5, 5),(3, 4, 1),(3, 4, 4),(3, 5, 1),(3, 6, 1),(4, 1, 3),(4, 7, 2),(5, 0, 7),(5, 2, 4),(5, 6, 4),(5, 7, 5),(6, 1, 7),(6, 2, 3),(6, 4, 2),(6, 6, 2),(7, 3, 5),(7, 7, 7)).map { case (x0,x1,x2) => DTuple(Atom(x0),Atom(x1),Atom(x2)) }
	val MputInstFldInstTuples = Set((4, 4, 3, 5)).map { case (x0,x1,x2,x3) => DTuple(Atom(x0),Atom(x1),Atom(x2),Atom(x3)) }
	val MputStatFldInstTuples = Set((4, 1, 1),(6, 5, 7),(7, 6, 7)).map { case (x0,x1,x2) => DTuple(Atom(x0),Atom(x1),Atom(x2)) }
	val MgetInstFldInstTuples = Set((4, 3, 2, 2)).map { case (x0,x1,x2,x3) => DTuple(Atom(x0),Atom(x1),Atom(x2),Atom(x3)) }
	val MITuples = Set((0, 0),(1, 1),(2, 2),(3, 3)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val VHTuples = Set((2, 2),(4, 4)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val IMTuples = Set((0, 1),(1, 2),(2, 3),(3, 4)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val MgetStatFldInstTuples = Set((4, 0, 0),(5, 6, 4),(7, 6, 6)).map { case (x0,x1,x2) => DTuple(Atom(x0),Atom(x1),Atom(x2)) }
	val refStatFieldTuples = Set((0, 0),(1, 0),(2, 0),(4, 0),(5, 4),(7, 6),(0, 4),(2, 3),(4, 4)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val modStatFieldTuples = Set((0, 1),(1, 1),(2, 1),(3, 1),(4, 1),(6, 5),(7, 6),(0, 2),(2, 6),(4, 3),(4, 5)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val rMMTuples = Set((0, 1),(0, 2),(0, 3),(0, 4),(1, 2),(1, 3),(1, 4),(2, 3),(2, 4),(3, 4)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val refInstFieldTuples = Set((0, 2, 2),(1, 2, 2),(2, 2, 2),(3, 2, 2),(4, 2, 2),(0, 4, 4),(0, 4, 7),(0, 6, 0),(1, 0, 0),(1, 0, 1),(1, 4, 4),(1, 7, 5),(2, 0, 7),(2, 3, 0),(2, 7, 4),(3, 0, 3),(3, 1, 2),(3, 1, 4),(3, 2, 6),(3, 3, 4),(3, 4, 7),(3, 5, 3),(3, 6, 1),(3, 7, 7),(4, 1, 2),(5, 4, 2),(6, 0, 5),(6, 2, 0),(6, 2, 6),(6, 3, 6),(6, 5, 4),(6, 6, 2),(7, 2, 3),(7, 4, 0),(7, 4, 3),(7, 5, 5),(7, 5, 7)).map { case (x0,x1,x2) => DTuple(Atom(x0),Atom(x1),Atom(x2)) }
override val edb = Config(
MputInstFldInst -> (Instance[FValue](MputInstFldInst) ++ MputInstFldInstTuples.map(t => t -> FValue.One).toMap),
MputStatFldInst -> (Instance[FValue](MputStatFldInst) ++ MputStatFldInstTuples.map(t => t -> FValue.One).toMap),
MgetInstFldInst -> (Instance[FValue](MgetInstFldInst) ++ MgetInstFldInstTuples.map(t => t -> FValue.One).toMap),
MI -> (Instance[FValue](MI) ++ MITuples.map(t => t -> FValue.One).toMap),
VH -> (Instance[FValue](VH) ++ VHTuples.map(t => t -> FValue.One).toMap),
IM -> (Instance[FValue](IM) ++ IMTuples.map(t => t -> FValue.One).toMap),
MgetStatFldInst -> (Instance[FValue](MgetStatFldInst) ++ MgetStatFldInstTuples.map(t => t -> FValue.One).toMap),
)
override val refOut = Config(
modStatField -> (Instance[FValue](modStatField) ++ modStatFieldTuples.map(t => t -> FValue.One).toMap),
refStatField -> (Instance[FValue](refStatField) ++ refStatFieldTuples.map(t => t -> FValue.One).toMap),
modInstField -> (Instance[FValue](modInstField) ++ modInstFieldTuples.map(t => t -> FValue.One).toMap),
refInstField -> (Instance[FValue](refInstField) ++ refInstFieldTuples.map(t => t -> FValue.One).toMap),
rMM -> (Instance[FValue](rMM) ++ rMMTuples.map(t => t -> FValue.One).toMap),
)
	val x1F = Variable("x1F",F)
	val x3M = Variable("x3M",M)
	val x2V = Variable("x2V",V)
	val x2H = Variable("x2H",H)
	val x2I = Variable("x2I",I)
	val x3V = Variable("x3V",V)
	val x2M = Variable("x2M",M)
	val x4V = Variable("x4V",V)
	val x1M = Variable("x1M",M)
	val x1H = Variable("x1H",H)
	val x2F = Variable("x2F",F)
	val x0M = Variable("x0M",M)
	val soup = Set(
		Rule(0, FValue(0.5, Token(0)), rMM(x0M,x1M),rMM(x0M,x2M),rMM(x2M,x1M)),
		Rule(1, FValue(0.5, Token(1)), rMM(x0M,x1M),IM(x2I,x1M),MI(x0M,x2I)),
		Rule(2, FValue(0.5, Token(2)), rMM(x0M,x1M),rMM(x2M,x0M),rMM(x2M,x3M),rMM(x3M,x1M)),
		Rule(3, FValue(0.5, Token(3)), rMM(x0M,x1M),rMM(x0M,x2M),rMM(x2M,x3M),rMM(x3M,x1M)),
		Rule(4, FValue(0.5, Token(4)), rMM(x0M,x1M),rMM(x0M,x2M),rMM(x1M,x2M)),
		Rule(5, FValue(0.5, Token(5)), rMM(x0M,x1M),refStatField(x0M,x2F),refStatField(x1M,x2F)),
		Rule(6, FValue(0.5, Token(6)), rMM(x0M,x1M),modStatField(x0M,x2F),modStatField(x1M,x2F)),
		Rule(7, FValue(0.5, Token(7)), rMM(x0M,x1M),MI(x0M,x2I),MI(x1M,x2I)),
		Rule(8, FValue(0.5, Token(8)), refStatField(x0M,x1F),rMM(x0M,x2M),refStatField(x2M,x1F)),
		Rule(9, FValue(0.5, Token(9)), refStatField(x0M,x1F),modStatField(x2M,x1F),rMM(x0M,x2M)),
		Rule(10, FValue(0.5, Token(10)), refStatField(x0M,x1F),refInstField(x0M,x2H,x1F)),
		Rule(11, FValue(0.5, Token(11)), refStatField(x0M,x1F),modInstField(x0M,x2H,x1F)),
		Rule(12, FValue(0.5, Token(12)), refStatField(x0M,x1F),MgetStatFldInst(x0M,x2V,x1F)),
		Rule(13, FValue(0.5, Token(13)), refStatField(x0M,x1F),modStatField(x0M,x1F)),
		Rule(14, FValue(0.5, Token(14)), refStatField(x0M,x1F),MputStatFldInst(x0M,x1F,x2V)),
		Rule(15, FValue(0.5, Token(15)), modStatField(x0M,x1F),rMM(x0M,x2M),refStatField(x2M,x1F)),
		Rule(16, FValue(0.5, Token(16)), modStatField(x0M,x1F),modStatField(x2M,x1F),rMM(x0M,x2M)),
		Rule(17, FValue(0.5, Token(17)), modStatField(x0M,x1F),refInstField(x0M,x2H,x1F)),
		Rule(18, FValue(0.5, Token(18)), modStatField(x0M,x1F),modInstField(x0M,x2H,x1F)),
		Rule(19, FValue(0.5, Token(19)), modStatField(x0M,x1F),MgetStatFldInst(x0M,x2V,x1F)),
		Rule(20, FValue(0.5, Token(20)), modStatField(x0M,x1F),refStatField(x0M,x1F)),
		Rule(21, FValue(0.5, Token(21)), modStatField(x0M,x1F),MputStatFldInst(x0M,x1F,x2V)),
		Rule(22, FValue(0.5, Token(22)), refInstField(x0M,x1H,x2F),MputInstFldInst(x0M,x3V,x2F,x4V),VH(x3V,x1H)),
		Rule(23, FValue(0.5, Token(23)), refInstField(x0M,x1H,x2F),modInstField(x0M,x1H,x2F)),
		Rule(24, FValue(0.5, Token(24)), refInstField(x0M,x1H,x2F),rMM(x0M,x3M),refInstField(x3M,x1H,x2F)),
		Rule(25, FValue(0.5, Token(25)), refInstField(x0M,x1H,x2F),MgetInstFldInst(x0M,x3V,x4V,x2F),VH(x4V,x1H)),
		Rule(26, FValue(0.5, Token(26)), modInstField(x0M,x1H,x2F),MputInstFldInst(x0M,x3V,x2F,x4V),VH(x3V,x1H)),
		Rule(27, FValue(0.5, Token(27)), modInstField(x0M,x1H,x2F),refInstField(x0M,x1H,x2F)),
		Rule(28, FValue(0.5, Token(28)), modInstField(x0M,x1H,x2F),modInstField(x3M,x1H,x2F),rMM(x0M,x3M)),
		Rule(29, FValue(0.5, Token(29)), modInstField(x0M,x1H,x2F),MgetInstFldInst(x0M,x3V,x4V,x2F),VH(x4V,x1H)),
	)
	val soupProg = Program("modrefSoup", soup)
	val evaluator = SeminaiveEvaluator(soupProg)

	override val expected: Set[Any] = Set(1, 2, 9, 13, 15, 17, 25, 26, 27, 29)
	override val maxVarCount: Int = 20
}
