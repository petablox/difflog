package qd
package learner
import org.scalatest.{FunSuite, Ignore}

class Genpolysite_10 extends Problem {
	override val name = "polysite"
	val ISet = Range(0, 17).map(i => Atom(i)).toSet
	val I = Domain("I", ISet)
	val CSet = Range(0, 1).map(i => Atom(i)).toSet
	val C = Domain("C", CSet)
	val MSet = Range(0, 12).map(i => Atom(i)).toSet
	val M = Domain("M", MSet)
	val CICM = Relation("CICM", C,I,M)
	val virtIM = Relation("virtIM", I,M)
	val Mneq = Relation("Mneq", M,M)
	val virtI = Relation("virtI", I)
	val insvIM = Relation("insvIM", I,M)
	val polySite = Relation("polySite", I)
	val CICMTuples = Set((0, 0, 10),(0, 8, 5),(0, 4, 8),(0, 12, 7),(0, 2, 1),(0, 10, 6),(0, 6, 7),(0, 14, 4),(0, 14, 6),(0, 1, 1),(0, 9, 7),(0, 5, 1),(0, 13, 4),(0, 3, 4),(0, 3, 2),(0, 3, 6),(0, 11, 3),(0, 7, 2),(0, 15, 7)).map { case (x0,x1,x2) => DTuple(Atom(x0),Atom(x1),Atom(x2)) }
	val virtIMTuples = Set((10, 2),(14, 2),(13, 2),(3, 2),(7, 2),(16, 11)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val virtITuples = Set(16,10,14,3,7).map { case (x0) => DTuple(Atom(x0)) }
	val MneqTuples = Set((1, 2),(1, 3),(1, 4),(1, 5),(1, 6),(1, 7),(1, 8),(1, 9),(1, 10),(1, 11),(2, 1),(2, 3),(2, 4),(2, 5),(2, 6),(2, 7),(2, 8),(2, 9),(2, 10),(2, 11),(3, 1),(3, 2),(3, 4),(3, 5),(3, 6),(3, 7),(3, 8),(3, 9),(3, 10),(3, 11),(4, 1),(4, 2),(4, 3),(4, 5),(4, 6),(4, 7),(4, 8),(4, 9),(4, 10),(4, 11),(5, 1),(5, 2),(5, 3),(5, 4),(5, 6),(5, 7),(5, 8),(5, 9),(5, 10),(5, 11),(6, 1),(6, 2),(6, 3),(6, 4),(6, 5),(6, 7),(6, 8),(6, 9),(6, 10),(6, 11),(7, 1),(7, 2),(7, 3),(7, 4),(7, 5),(7, 6),(7, 8),(7, 9),(7, 10),(7, 11),(8, 1),(8, 2),(8, 3),(8, 4),(8, 5),(8, 6),(8, 7),(8, 9),(8, 10),(8, 11),(9, 1),(9, 2),(9, 3),(9, 4),(9, 5),(9, 6),(9, 7),(9, 8),(9, 10),(9, 11),(10, 1),(10, 2),(10, 3),(10, 4),(10, 5),(10, 6),(10, 7),(10, 8),(10, 9),(10, 11),(11, 1),(11, 2),(11, 3),(11, 4),(11, 5),(11, 6),(11, 7),(11, 8),(11, 9),(11, 10)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val insvIMTuples = Set((0, 10),(4, 8),(12, 7),(2, 1),(10, 6),(6, 7),(14, 4),(14, 6),(1, 1),(9, 7),(5, 1),(13, 4),(3, 4),(3, 2),(3, 6),(11, 3),(7, 2),(0, 9),(2, 3),(4, 1),(4, 4),(5, 5),(5, 10),(7, 6),(7, 7),(8, 8),(8, 9),(9, 6),(13, 5),(14, 7),(14, 8),(15, 9)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val polySiteTuples = Set(14,3,1,2,7).map { case (x0) => DTuple(Atom(x0)) }
override val edb = Config(
CICM -> (Instance[FValue](CICM) ++ CICMTuples.map(t => t -> FValue.One).toMap),
virtIM -> (Instance[FValue](virtIM) ++ virtIMTuples.map(t => t -> FValue.One).toMap),
Mneq -> (Instance[FValue](Mneq) ++ MneqTuples.map(t => t -> FValue.One).toMap),
)
override val refOut = Config(
virtI -> (Instance[FValue](virtI) ++ virtITuples.map(t => t -> FValue.One).toMap),
insvIM -> (Instance[FValue](insvIM) ++ insvIMTuples.map(t => t -> FValue.One).toMap),
polySite -> (Instance[FValue](polySite) ++ polySiteTuples.map(t => t -> FValue.One).toMap),
)
	val x4M = Variable("x4M",M)
	val x2I = Variable("x2I",I)
	val x3C = Variable("x3C",C)
	val x4I = Variable("x4I",I)
	val x3M = Variable("x3M",M)
	val x2C = Variable("x2C",C)
	val x3I = Variable("x3I",I)
	val x4C = Variable("x4C",C)
	val x5M = Variable("x5M",M)
	val x0C = Variable("x0C",C)
	val x2M = Variable("x2M",M)
	val x1M = Variable("x1M",M)
	val x0I = Variable("x0I",I)
	val x5I = Variable("x5I",I)
	val x1I = Variable("x1I",I)
	val x0M = Variable("x0M",M)
	val soup = Set(
		Rule(0, FValue(0.5, Token(0)), virtI(x0I),insvIM(x0I,x1M)),
		Rule(1, FValue(0.5, Token(1)), virtI(x0I),virtIM(x0I,x1M)),
		Rule(2, FValue(0.5, Token(2)), virtI(x1I),CICM(x0C,x1I,x2M)),
		Rule(3, FValue(0.5, Token(3)), virtI(x0I),polySite(x0I)),
		Rule(4, FValue(0.5, Token(4)), virtI(x3I),insvIM(x0I,x1M),insvIM(x3I,x2M),virtIM(x0I,x2M)),
		Rule(5, FValue(0.5, Token(5)), virtI(x3I),insvIM(x0I,x1M),virtIM(x0I,x2M),virtIM(x3I,x2M)),
		Rule(6, FValue(0.5, Token(6)), virtI(x3I),insvIM(x0I,x2M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
		Rule(7, FValue(0.5, Token(7)), virtI(x3I),insvIM(x0I,x2M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
		Rule(8, FValue(0.5, Token(8)), virtI(x3I),Mneq(x0M,x1M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
		Rule(9, FValue(0.5, Token(9)), virtI(x3I),Mneq(x0M,x1M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
		Rule(10, FValue(0.5, Token(10)), virtI(x3I),Mneq(x1M,x2M),insvIM(x0I,x1M),insvIM(x3I,x2M)),
		Rule(11, FValue(0.5, Token(11)), virtI(x3I),Mneq(x1M,x2M),insvIM(x0I,x1M),virtIM(x3I,x2M)),
		Rule(12, FValue(0.5, Token(12)), virtI(x3I),Mneq(x0M,x1M),Mneq(x1M,x2M),insvIM(x3I,x2M)),
		Rule(13, FValue(0.5, Token(13)), virtI(x3I),Mneq(x0M,x1M),Mneq(x1M,x2M),virtIM(x3I,x2M)),
		Rule(14, FValue(0.5, Token(14)), virtI(x3I),Mneq(x1M,x2M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
		Rule(15, FValue(0.5, Token(15)), virtI(x3I),Mneq(x1M,x2M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
		Rule(16, FValue(0.5, Token(16)), virtI(x3I),Mneq(x2M,x1M),insvIM(x0I,x1M),insvIM(x3I,x2M)),
		Rule(17, FValue(0.5, Token(17)), virtI(x3I),Mneq(x2M,x1M),insvIM(x0I,x1M),virtIM(x3I,x2M)),
		Rule(18, FValue(0.5, Token(18)), virtI(x3I),Mneq(x2M,x1M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
		Rule(19, FValue(0.5, Token(19)), virtI(x3I),Mneq(x2M,x1M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
		Rule(20, FValue(0.5, Token(20)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x2M,x0M)),
		Rule(21, FValue(0.5, Token(21)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x0I,x1M)),
		Rule(22, FValue(0.5, Token(22)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x1M,x2M)),
		Rule(23, FValue(0.5, Token(23)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x0I,x1M)),
		Rule(24, FValue(0.5, Token(24)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x0I,x1M)),
		Rule(25, FValue(0.5, Token(25)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x0I,x1M)),
		Rule(26, FValue(0.5, Token(26)), virtI(x2I),CICM(x3C,x2I,x4M),Mneq(x0M,x1M),insvIM(x2I,x0M)),
		Rule(27, FValue(0.5, Token(27)), virtI(x2I),CICM(x3C,x2I,x4M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
		Rule(28, FValue(0.5, Token(28)), virtI(x2I),Mneq(x0M,x1M),insvIM(x2I,x0M),polySite(x2I)),
		Rule(29, FValue(0.5, Token(29)), virtI(x2I),Mneq(x0M,x1M),polySite(x2I),virtIM(x2I,x0M)),
		Rule(30, FValue(0.5, Token(30)), virtI(x3I),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),insvIM(x3I,x4M)),
		Rule(31, FValue(0.5, Token(31)), virtI(x3I),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),virtIM(x3I,x4M)),
		Rule(32, FValue(0.5, Token(32)), virtI(x3I),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),polySite(x3I)),
		Rule(33, FValue(0.5, Token(33)), virtI(x0I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x0I,x1M)),
		Rule(34, FValue(0.5, Token(34)), virtI(x0I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x0I,x1M)),
		Rule(35, FValue(0.5, Token(35)), virtI(x0I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x0I,x1M)),
		Rule(36, FValue(0.5, Token(36)), virtI(x0I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x0I,x1M)),
		Rule(37, FValue(0.5, Token(37)), virtI(x0I),CICM(x3C,x2I,x4M),insvIM(x0I,x1M),insvIM(x2I,x1M)),
		Rule(38, FValue(0.5, Token(38)), virtI(x0I),CICM(x3C,x2I,x4M),insvIM(x0I,x1M),virtIM(x2I,x1M)),
		Rule(39, FValue(0.5, Token(39)), virtI(x0I),CICM(x3C,x2I,x4M),insvIM(x2I,x1M),virtIM(x0I,x1M)),
		Rule(40, FValue(0.5, Token(40)), virtI(x0I),CICM(x3C,x2I,x4M),virtIM(x0I,x1M),virtIM(x2I,x1M)),
		Rule(41, FValue(0.5, Token(41)), virtI(x0I),insvIM(x0I,x1M),insvIM(x2I,x1M),virtI(x2I)),
		Rule(42, FValue(0.5, Token(42)), virtI(x0I),insvIM(x0I,x1M),insvIM(x2I,x1M),polySite(x2I)),
		Rule(43, FValue(0.5, Token(43)), virtI(x0I),insvIM(x0I,x1M),virtI(x2I),virtIM(x2I,x1M)),
		Rule(44, FValue(0.5, Token(44)), virtI(x0I),insvIM(x0I,x1M),polySite(x2I),virtIM(x2I,x1M)),
		Rule(45, FValue(0.5, Token(45)), virtI(x0I),insvIM(x2I,x1M),virtI(x2I),virtIM(x0I,x1M)),
		Rule(46, FValue(0.5, Token(46)), virtI(x0I),insvIM(x2I,x1M),polySite(x2I),virtIM(x0I,x1M)),
		Rule(47, FValue(0.5, Token(47)), virtI(x0I),virtI(x2I),virtIM(x0I,x1M),virtIM(x2I,x1M)),
		Rule(48, FValue(0.5, Token(48)), virtI(x0I),polySite(x2I),virtIM(x0I,x1M),virtIM(x2I,x1M)),
		Rule(49, FValue(0.5, Token(49)), virtI(x4I),CICM(x2C,x0I,x3M),insvIM(x0I,x1M),insvIM(x4I,x3M)),
		Rule(50, FValue(0.5, Token(50)), virtI(x4I),CICM(x2C,x0I,x3M),insvIM(x0I,x1M),virtIM(x4I,x3M)),
		Rule(51, FValue(0.5, Token(51)), virtI(x4I),CICM(x2C,x0I,x3M),insvIM(x4I,x3M),virtIM(x0I,x1M)),
		Rule(52, FValue(0.5, Token(52)), virtI(x4I),CICM(x2C,x0I,x3M),virtIM(x0I,x1M),virtIM(x4I,x3M)),
		Rule(53, FValue(0.5, Token(53)), virtI(x3I),Mneq(x0M,x0M),Mneq(x0M,x2M),insvIM(x3I,x2M)),
		Rule(54, FValue(0.5, Token(54)), virtI(x3I),Mneq(x0M,x0M),Mneq(x0M,x2M),virtIM(x3I,x2M)),
		Rule(55, FValue(0.5, Token(55)), virtI(x3I),Mneq(x0M,x2M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
		Rule(56, FValue(0.5, Token(56)), virtI(x3I),Mneq(x0M,x2M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
		Rule(57, FValue(0.5, Token(57)), virtI(x3I),Mneq(x0M,x0M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
		Rule(58, FValue(0.5, Token(58)), virtI(x3I),Mneq(x0M,x0M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
		Rule(59, FValue(0.5, Token(59)), virtI(x3I),Mneq(x1M,x2M),insvIM(x3I,x1M),insvIM(x3I,x2M)),
		Rule(60, FValue(0.5, Token(60)), virtI(x3I),Mneq(x1M,x2M),insvIM(x3I,x1M),virtIM(x3I,x2M)),
		Rule(61, FValue(0.5, Token(61)), virtI(x3I),Mneq(x1M,x2M),insvIM(x3I,x2M),virtIM(x3I,x1M)),
		Rule(62, FValue(0.5, Token(62)), virtI(x3I),Mneq(x1M,x2M),virtIM(x3I,x1M),virtIM(x3I,x2M)),
		Rule(63, FValue(0.5, Token(63)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x0M,x2M),Mneq(x2M,x0M)),
		Rule(64, FValue(0.5, Token(64)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x0M,x0M),Mneq(x2M,x0M)),
		Rule(65, FValue(0.5, Token(65)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x4I,x1M)),
		Rule(66, FValue(0.5, Token(66)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x4I,x1M)),
		Rule(67, FValue(0.5, Token(67)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x4I,x1M)),
		Rule(68, FValue(0.5, Token(68)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x4I,x1M)),
		Rule(69, FValue(0.5, Token(69)), virtI(x2I),Mneq(x0M,x0M),insvIM(x2I,x0M),virtIM(x2I,x3M)),
		Rule(70, FValue(0.5, Token(70)), virtI(x2I),Mneq(x0M,x0M),insvIM(x2I,x3M),virtIM(x2I,x0M)),
		Rule(71, FValue(0.5, Token(71)), virtI(x2I),CICM(x3C,x2I,x4M),Mneq(x0M,x0M),insvIM(x2I,x0M)),
		Rule(72, FValue(0.5, Token(72)), virtI(x2I),CICM(x3C,x2I,x4M),Mneq(x0M,x0M),virtIM(x2I,x0M)),
		Rule(73, FValue(0.5, Token(73)), virtI(x2I),CICM(x3C,x2I,x0M),Mneq(x0M,x1M),insvIM(x2I,x0M)),
		Rule(74, FValue(0.5, Token(74)), virtI(x2I),CICM(x3C,x2I,x0M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
		Rule(75, FValue(0.5, Token(75)), virtI(x0I),CICM(x3C,x2I,x1M),insvIM(x0I,x1M),insvIM(x2I,x1M)),
		Rule(76, FValue(0.5, Token(76)), virtI(x0I),CICM(x3C,x2I,x1M),insvIM(x0I,x1M),virtIM(x2I,x1M)),
		Rule(77, FValue(0.5, Token(77)), virtI(x0I),CICM(x3C,x2I,x1M),insvIM(x2I,x1M),virtIM(x0I,x1M)),
		Rule(78, FValue(0.5, Token(78)), virtI(x0I),CICM(x3C,x2I,x1M),virtIM(x0I,x1M),virtIM(x2I,x1M)),
		Rule(79, FValue(0.5, Token(79)), polySite(x0I),insvIM(x0I,x1M)),
		Rule(80, FValue(0.5, Token(80)), polySite(x0I),virtIM(x0I,x1M)),
		Rule(81, FValue(0.5, Token(81)), polySite(x1I),CICM(x0C,x1I,x2M)),
		Rule(82, FValue(0.5, Token(82)), polySite(x0I),virtI(x0I)),
		Rule(83, FValue(0.5, Token(83)), polySite(x3I),insvIM(x0I,x1M),insvIM(x3I,x2M),virtIM(x0I,x2M)),
		Rule(84, FValue(0.5, Token(84)), polySite(x3I),insvIM(x0I,x1M),virtIM(x0I,x2M),virtIM(x3I,x2M)),
		Rule(85, FValue(0.5, Token(85)), polySite(x3I),insvIM(x0I,x2M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
		Rule(86, FValue(0.5, Token(86)), polySite(x3I),insvIM(x0I,x2M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
		Rule(87, FValue(0.5, Token(87)), polySite(x3I),Mneq(x0M,x1M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
		Rule(88, FValue(0.5, Token(88)), polySite(x3I),Mneq(x0M,x1M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
		Rule(89, FValue(0.5, Token(89)), polySite(x3I),Mneq(x1M,x2M),insvIM(x0I,x1M),insvIM(x3I,x2M)),
		Rule(90, FValue(0.5, Token(90)), polySite(x3I),Mneq(x1M,x2M),insvIM(x0I,x1M),virtIM(x3I,x2M)),
		Rule(91, FValue(0.5, Token(91)), polySite(x3I),Mneq(x0M,x1M),Mneq(x1M,x2M),insvIM(x3I,x2M)),
		Rule(92, FValue(0.5, Token(92)), polySite(x3I),Mneq(x0M,x1M),Mneq(x1M,x2M),virtIM(x3I,x2M)),
		Rule(93, FValue(0.5, Token(93)), polySite(x3I),Mneq(x1M,x2M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
		Rule(94, FValue(0.5, Token(94)), polySite(x3I),Mneq(x1M,x2M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
		Rule(95, FValue(0.5, Token(95)), polySite(x3I),Mneq(x2M,x1M),insvIM(x0I,x1M),insvIM(x3I,x2M)),
		Rule(96, FValue(0.5, Token(96)), polySite(x3I),Mneq(x2M,x1M),insvIM(x0I,x1M),virtIM(x3I,x2M)),
		Rule(97, FValue(0.5, Token(97)), polySite(x3I),Mneq(x2M,x1M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
		Rule(98, FValue(0.5, Token(98)), polySite(x3I),Mneq(x2M,x1M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
		Rule(99, FValue(0.5, Token(99)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x2M,x0M)),
		Rule(100, FValue(0.5, Token(100)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x0I,x1M)),
		Rule(101, FValue(0.5, Token(101)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x1M,x2M)),
		Rule(102, FValue(0.5, Token(102)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x0I,x1M)),
		Rule(103, FValue(0.5, Token(103)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x0I,x1M)),
		Rule(104, FValue(0.5, Token(104)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x0I,x1M)),
		Rule(105, FValue(0.5, Token(105)), polySite(x2I),CICM(x3C,x2I,x4M),Mneq(x0M,x1M),insvIM(x2I,x0M)),
		Rule(106, FValue(0.5, Token(106)), polySite(x2I),CICM(x3C,x2I,x4M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
		Rule(107, FValue(0.5, Token(107)), polySite(x2I),Mneq(x0M,x1M),insvIM(x2I,x0M),virtI(x2I)),
		Rule(108, FValue(0.5, Token(108)), polySite(x2I),Mneq(x0M,x1M),virtI(x2I),virtIM(x2I,x0M)),
		Rule(109, FValue(0.5, Token(109)), polySite(x3I),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),insvIM(x3I,x4M)),
		Rule(110, FValue(0.5, Token(110)), polySite(x3I),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),virtIM(x3I,x4M)),
		Rule(111, FValue(0.5, Token(111)), polySite(x3I),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),virtI(x3I)),
		Rule(112, FValue(0.5, Token(112)), polySite(x0I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x0I,x1M)),
		Rule(113, FValue(0.5, Token(113)), polySite(x0I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x0I,x1M)),
		Rule(114, FValue(0.5, Token(114)), polySite(x0I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x0I,x1M)),
		Rule(115, FValue(0.5, Token(115)), polySite(x0I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x0I,x1M)),
		Rule(116, FValue(0.5, Token(116)), polySite(x0I),CICM(x3C,x2I,x4M),insvIM(x0I,x1M),insvIM(x2I,x1M)),
		Rule(117, FValue(0.5, Token(117)), polySite(x0I),CICM(x3C,x2I,x4M),insvIM(x0I,x1M),virtIM(x2I,x1M)),
		Rule(118, FValue(0.5, Token(118)), polySite(x0I),CICM(x3C,x2I,x4M),insvIM(x2I,x1M),virtIM(x0I,x1M)),
		Rule(119, FValue(0.5, Token(119)), polySite(x0I),CICM(x3C,x2I,x4M),virtIM(x0I,x1M),virtIM(x2I,x1M)),
		Rule(120, FValue(0.5, Token(120)), polySite(x0I),insvIM(x0I,x1M),insvIM(x2I,x1M),virtI(x2I)),
		Rule(121, FValue(0.5, Token(121)), polySite(x0I),insvIM(x0I,x1M),insvIM(x2I,x1M),polySite(x2I)),
		Rule(122, FValue(0.5, Token(122)), polySite(x0I),insvIM(x0I,x1M),virtI(x2I),virtIM(x2I,x1M)),
		Rule(123, FValue(0.5, Token(123)), polySite(x0I),insvIM(x0I,x1M),polySite(x2I),virtIM(x2I,x1M)),
		Rule(124, FValue(0.5, Token(124)), polySite(x0I),insvIM(x2I,x1M),virtI(x2I),virtIM(x0I,x1M)),
		Rule(125, FValue(0.5, Token(125)), polySite(x0I),insvIM(x2I,x1M),polySite(x2I),virtIM(x0I,x1M)),
		Rule(126, FValue(0.5, Token(126)), polySite(x0I),virtI(x2I),virtIM(x0I,x1M),virtIM(x2I,x1M)),
		Rule(127, FValue(0.5, Token(127)), polySite(x0I),polySite(x2I),virtIM(x0I,x1M),virtIM(x2I,x1M)),
		Rule(128, FValue(0.5, Token(128)), polySite(x4I),CICM(x2C,x0I,x3M),insvIM(x0I,x1M),insvIM(x4I,x3M)),
		Rule(129, FValue(0.5, Token(129)), polySite(x4I),CICM(x2C,x0I,x3M),insvIM(x0I,x1M),virtIM(x4I,x3M)),
		Rule(130, FValue(0.5, Token(130)), polySite(x4I),CICM(x2C,x0I,x3M),insvIM(x4I,x3M),virtIM(x0I,x1M)),
		Rule(131, FValue(0.5, Token(131)), polySite(x4I),CICM(x2C,x0I,x3M),virtIM(x0I,x1M),virtIM(x4I,x3M)),
		Rule(132, FValue(0.5, Token(132)), polySite(x3I),Mneq(x0M,x0M),Mneq(x0M,x2M),insvIM(x3I,x2M)),
		Rule(133, FValue(0.5, Token(133)), polySite(x3I),Mneq(x0M,x0M),Mneq(x0M,x2M),virtIM(x3I,x2M)),
		Rule(134, FValue(0.5, Token(134)), polySite(x3I),Mneq(x0M,x2M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
		Rule(135, FValue(0.5, Token(135)), polySite(x3I),Mneq(x0M,x2M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
		Rule(136, FValue(0.5, Token(136)), polySite(x3I),Mneq(x0M,x0M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
		Rule(137, FValue(0.5, Token(137)), polySite(x3I),Mneq(x0M,x0M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
		Rule(138, FValue(0.5, Token(138)), polySite(x3I),Mneq(x1M,x2M),insvIM(x3I,x1M),insvIM(x3I,x2M)),
		Rule(139, FValue(0.5, Token(139)), polySite(x3I),Mneq(x1M,x2M),insvIM(x3I,x1M),virtIM(x3I,x2M)),
		Rule(140, FValue(0.5, Token(140)), polySite(x3I),Mneq(x1M,x2M),insvIM(x3I,x2M),virtIM(x3I,x1M)),
		Rule(141, FValue(0.5, Token(141)), polySite(x3I),Mneq(x1M,x2M),virtIM(x3I,x1M),virtIM(x3I,x2M)),
		Rule(142, FValue(0.5, Token(142)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x0M,x2M),Mneq(x2M,x0M)),
		Rule(143, FValue(0.5, Token(143)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x0M,x0M),Mneq(x2M,x0M)),
		Rule(144, FValue(0.5, Token(144)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x4I,x1M)),
		Rule(145, FValue(0.5, Token(145)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x4I,x1M)),
		Rule(146, FValue(0.5, Token(146)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x4I,x1M)),
		Rule(147, FValue(0.5, Token(147)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x4I,x1M)),
		Rule(148, FValue(0.5, Token(148)), polySite(x2I),Mneq(x0M,x0M),insvIM(x2I,x0M),virtIM(x2I,x3M)),
		Rule(149, FValue(0.5, Token(149)), polySite(x2I),Mneq(x0M,x0M),insvIM(x2I,x3M),virtIM(x2I,x0M)),
		Rule(150, FValue(0.5, Token(150)), polySite(x2I),CICM(x3C,x2I,x4M),Mneq(x0M,x0M),insvIM(x2I,x0M)),
		Rule(151, FValue(0.5, Token(151)), polySite(x2I),CICM(x3C,x2I,x4M),Mneq(x0M,x0M),virtIM(x2I,x0M)),
		Rule(152, FValue(0.5, Token(152)), polySite(x2I),CICM(x3C,x2I,x0M),Mneq(x0M,x1M),insvIM(x2I,x0M)),
		Rule(153, FValue(0.5, Token(153)), polySite(x2I),CICM(x3C,x2I,x0M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
		Rule(154, FValue(0.5, Token(154)), polySite(x0I),CICM(x3C,x2I,x1M),insvIM(x0I,x1M),insvIM(x2I,x1M)),
		Rule(155, FValue(0.5, Token(155)), polySite(x0I),CICM(x3C,x2I,x1M),insvIM(x0I,x1M),virtIM(x2I,x1M)),
		Rule(156, FValue(0.5, Token(156)), polySite(x0I),CICM(x3C,x2I,x1M),insvIM(x2I,x1M),virtIM(x0I,x1M)),
		Rule(157, FValue(0.5, Token(157)), polySite(x0I),CICM(x3C,x2I,x1M),virtIM(x0I,x1M),virtIM(x2I,x1M)),
		Rule(158, FValue(0.5, Token(158)), insvIM(x0I,x1M),virtIM(x0I,x1M)),
		Rule(159, FValue(0.5, Token(159)), insvIM(x1I,x2M),CICM(x0C,x1I,x2M)),
		Rule(160, FValue(0.5, Token(160)), insvIM(x3I,x1M),insvIM(x0I,x1M),insvIM(x0I,x2M),insvIM(x3I,x2M)),
		Rule(161, FValue(0.5, Token(161)), insvIM(x3I,x1M),insvIM(x0I,x1M),insvIM(x0I,x2M),virtIM(x3I,x2M)),
		Rule(162, FValue(0.5, Token(162)), insvIM(x3I,x1M),insvIM(x0I,x1M),insvIM(x3I,x2M),virtIM(x0I,x2M)),
		Rule(163, FValue(0.5, Token(163)), insvIM(x3I,x1M),insvIM(x0I,x1M),virtIM(x0I,x2M),virtIM(x3I,x2M)),
		Rule(164, FValue(0.5, Token(164)), insvIM(x3I,x1M),Mneq(x0M,x1M),Mneq(x0M,x2M),insvIM(x3I,x2M)),
		Rule(165, FValue(0.5, Token(165)), insvIM(x3I,x1M),Mneq(x0M,x1M),Mneq(x0M,x2M),virtIM(x3I,x2M)),
		Rule(166, FValue(0.5, Token(166)), insvIM(x3I,x1M),insvIM(x0I,x2M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
		Rule(167, FValue(0.5, Token(167)), insvIM(x3I,x1M),insvIM(x0I,x2M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
		Rule(168, FValue(0.5, Token(168)), insvIM(x3I,x1M),insvIM(x3I,x2M),virtIM(x0I,x1M),virtIM(x0I,x2M)),
		Rule(169, FValue(0.5, Token(169)), insvIM(x3I,x1M),virtIM(x0I,x1M),virtIM(x0I,x2M),virtIM(x3I,x2M)),
		Rule(170, FValue(0.5, Token(170)), insvIM(x3I,x0M),Mneq(x0M,x1M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
		Rule(171, FValue(0.5, Token(171)), insvIM(x3I,x0M),Mneq(x0M,x1M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
		Rule(172, FValue(0.5, Token(172)), insvIM(x3I,x1M),Mneq(x0M,x1M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
		Rule(173, FValue(0.5, Token(173)), insvIM(x3I,x1M),Mneq(x0M,x1M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
		Rule(174, FValue(0.5, Token(174)), insvIM(x3I,x0M),Mneq(x0M,x1M),Mneq(x1M,x2M),insvIM(x3I,x2M)),
		Rule(175, FValue(0.5, Token(175)), insvIM(x3I,x0M),Mneq(x0M,x1M),Mneq(x1M,x2M),virtIM(x3I,x2M)),
		Rule(176, FValue(0.5, Token(176)), insvIM(x3I,x1M),Mneq(x1M,x2M),insvIM(x0I,x1M),insvIM(x3I,x2M)),
		Rule(177, FValue(0.5, Token(177)), insvIM(x3I,x1M),Mneq(x1M,x2M),insvIM(x0I,x1M),virtIM(x3I,x2M)),
		Rule(178, FValue(0.5, Token(178)), insvIM(x3I,x1M),Mneq(x0M,x1M),Mneq(x1M,x2M),insvIM(x3I,x2M)),
		Rule(179, FValue(0.5, Token(179)), insvIM(x3I,x1M),Mneq(x0M,x1M),Mneq(x1M,x2M),virtIM(x3I,x2M)),
		Rule(180, FValue(0.5, Token(180)), insvIM(x3I,x1M),Mneq(x1M,x2M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
		Rule(181, FValue(0.5, Token(181)), insvIM(x3I,x1M),Mneq(x1M,x2M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
		Rule(182, FValue(0.5, Token(182)), insvIM(x3I,x0M),Mneq(x0M,x1M),Mneq(x2M,x1M),insvIM(x3I,x2M)),
		Rule(183, FValue(0.5, Token(183)), insvIM(x3I,x0M),Mneq(x0M,x1M),Mneq(x2M,x1M),virtIM(x3I,x2M)),
		Rule(184, FValue(0.5, Token(184)), insvIM(x3I,x1M),Mneq(x2M,x1M),insvIM(x0I,x1M),insvIM(x3I,x2M)),
		Rule(185, FValue(0.5, Token(185)), insvIM(x3I,x1M),Mneq(x2M,x1M),insvIM(x0I,x1M),virtIM(x3I,x2M)),
		Rule(186, FValue(0.5, Token(186)), insvIM(x3I,x1M),Mneq(x2M,x1M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
		Rule(187, FValue(0.5, Token(187)), insvIM(x3I,x1M),Mneq(x2M,x1M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
		Rule(188, FValue(0.5, Token(188)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),insvIM(x0I,x1M),insvIM(x0I,x2M)),
		Rule(189, FValue(0.5, Token(189)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),insvIM(x0I,x1M),virtIM(x0I,x2M)),
		Rule(190, FValue(0.5, Token(190)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x0M,x2M)),
		Rule(191, FValue(0.5, Token(191)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),insvIM(x0I,x2M),virtIM(x0I,x1M)),
		Rule(192, FValue(0.5, Token(192)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),virtIM(x0I,x1M),virtIM(x0I,x2M)),
		Rule(193, FValue(0.5, Token(193)), insvIM(x4I,x0M),CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x2M,x0M)),
		Rule(194, FValue(0.5, Token(194)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x2M,x0M)),
		Rule(195, FValue(0.5, Token(195)), insvIM(x4I,x0M),CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x1M,x2M)),
		Rule(196, FValue(0.5, Token(196)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x0I,x1M)),
		Rule(197, FValue(0.5, Token(197)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x1M,x2M)),
		Rule(198, FValue(0.5, Token(198)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x0I,x1M)),
		Rule(199, FValue(0.5, Token(199)), insvIM(x4I,x0M),CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x2M,x1M)),
		Rule(200, FValue(0.5, Token(200)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x0I,x1M)),
		Rule(201, FValue(0.5, Token(201)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x0I,x1M)),
		Rule(202, FValue(0.5, Token(202)), insvIM(x2I,x0M),Mneq(x0M,x1M),insvIM(x2I,x3M),virtIM(x2I,x0M)),
		Rule(203, FValue(0.5, Token(203)), insvIM(x2I,x1M),insvIM(x0I,x1M),insvIM(x2I,x3M),virtIM(x2I,x1M)),
		Rule(204, FValue(0.5, Token(204)), insvIM(x2I,x1M),Mneq(x0M,x1M),insvIM(x2I,x3M),virtIM(x2I,x1M)),
		Rule(205, FValue(0.5, Token(205)), insvIM(x2I,x0M),CICM(x3C,x2I,x4M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
		Rule(206, FValue(0.5, Token(206)), insvIM(x2I,x1M),CICM(x3C,x2I,x4M),Mneq(x0M,x1M),insvIM(x2I,x0M)),
		Rule(207, FValue(0.5, Token(207)), insvIM(x2I,x1M),CICM(x3C,x2I,x4M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
		Rule(208, FValue(0.5, Token(208)), insvIM(x2I,x1M),CICM(x3C,x2I,x4M),insvIM(x0I,x1M),virtIM(x2I,x1M)),
		Rule(209, FValue(0.5, Token(209)), insvIM(x2I,x1M),CICM(x3C,x2I,x4M),Mneq(x0M,x1M),virtIM(x2I,x1M)),
		Rule(210, FValue(0.5, Token(210)), insvIM(x2I,x0M),Mneq(x0M,x1M),virtI(x2I),virtIM(x2I,x0M)),
		Rule(211, FValue(0.5, Token(211)), insvIM(x2I,x0M),Mneq(x0M,x1M),polySite(x2I),virtIM(x2I,x0M)),
		Rule(212, FValue(0.5, Token(212)), insvIM(x2I,x1M),Mneq(x0M,x1M),insvIM(x2I,x0M),virtI(x2I)),
		Rule(213, FValue(0.5, Token(213)), insvIM(x2I,x1M),Mneq(x0M,x1M),insvIM(x2I,x0M),polySite(x2I)),
		Rule(214, FValue(0.5, Token(214)), insvIM(x2I,x1M),Mneq(x0M,x1M),virtI(x2I),virtIM(x2I,x0M)),
		Rule(215, FValue(0.5, Token(215)), insvIM(x2I,x1M),Mneq(x0M,x1M),polySite(x2I),virtIM(x2I,x0M)),
		Rule(216, FValue(0.5, Token(216)), insvIM(x2I,x1M),insvIM(x0I,x1M),virtI(x2I),virtIM(x2I,x1M)),
		Rule(217, FValue(0.5, Token(217)), insvIM(x2I,x1M),insvIM(x0I,x1M),polySite(x2I),virtIM(x2I,x1M)),
		Rule(218, FValue(0.5, Token(218)), insvIM(x2I,x1M),Mneq(x0M,x1M),virtI(x2I),virtIM(x2I,x1M)),
		Rule(219, FValue(0.5, Token(219)), insvIM(x2I,x1M),Mneq(x0M,x1M),polySite(x2I),virtIM(x2I,x1M)),
		Rule(220, FValue(0.5, Token(220)), insvIM(x3I,x0M),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),insvIM(x3I,x4M)),
		Rule(221, FValue(0.5, Token(221)), insvIM(x3I,x0M),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),virtIM(x3I,x4M)),
		Rule(222, FValue(0.5, Token(222)), insvIM(x3I,x1M),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),insvIM(x3I,x4M)),
		Rule(223, FValue(0.5, Token(223)), insvIM(x3I,x1M),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),virtIM(x3I,x4M)),
		Rule(224, FValue(0.5, Token(224)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),insvIM(x0I,x1M),insvIM(x3I,x4M)),
		Rule(225, FValue(0.5, Token(225)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),insvIM(x0I,x1M),virtIM(x3I,x4M)),
		Rule(226, FValue(0.5, Token(226)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),Mneq(x0M,x1M),insvIM(x3I,x4M)),
		Rule(227, FValue(0.5, Token(227)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),Mneq(x0M,x1M),virtIM(x3I,x4M)),
		Rule(228, FValue(0.5, Token(228)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),insvIM(x3I,x4M),virtIM(x0I,x1M)),
		Rule(229, FValue(0.5, Token(229)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),virtIM(x0I,x1M),virtIM(x3I,x4M)),
		Rule(230, FValue(0.5, Token(230)), insvIM(x4I,x1M),CICM(x2C,x3I,x0M),CICM(x2C,x4I,x5M),Mneq(x0M,x1M)),
		Rule(231, FValue(0.5, Token(231)), insvIM(x3I,x0M),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),virtI(x3I)),
		Rule(232, FValue(0.5, Token(232)), insvIM(x3I,x0M),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),polySite(x3I)),
		Rule(233, FValue(0.5, Token(233)), insvIM(x3I,x1M),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),virtI(x3I)),
		Rule(234, FValue(0.5, Token(234)), insvIM(x3I,x1M),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),polySite(x3I)),
		Rule(235, FValue(0.5, Token(235)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),insvIM(x0I,x1M),virtI(x3I)),
		Rule(236, FValue(0.5, Token(236)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),insvIM(x0I,x1M),polySite(x3I)),
		Rule(237, FValue(0.5, Token(237)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),Mneq(x0M,x1M),virtI(x3I)),
		Rule(238, FValue(0.5, Token(238)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),Mneq(x0M,x1M),polySite(x3I)),
		Rule(239, FValue(0.5, Token(239)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),virtI(x3I),virtIM(x0I,x1M)),
		Rule(240, FValue(0.5, Token(240)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),polySite(x3I),virtIM(x0I,x1M)),
		Rule(241, FValue(0.5, Token(241)), insvIM(x0I,x2M),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x0I,x1M)),
		Rule(242, FValue(0.5, Token(242)), insvIM(x0I,x2M),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x0I,x1M)),
		Rule(243, FValue(0.5, Token(243)), insvIM(x0I,x2M),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x0I,x1M)),
		Rule(244, FValue(0.5, Token(244)), insvIM(x0I,x2M),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x0I,x1M)),
		Rule(245, FValue(0.5, Token(245)), insvIM(x0I,x2M),CICM(x3C,x4I,x2M),insvIM(x0I,x1M),virtIM(x0I,x2M)),
		Rule(246, FValue(0.5, Token(246)), insvIM(x0I,x4M),CICM(x3C,x2I,x4M),insvIM(x0I,x1M),insvIM(x2I,x1M)),
		Rule(247, FValue(0.5, Token(247)), insvIM(x0I,x4M),CICM(x3C,x2I,x4M),insvIM(x0I,x1M),virtIM(x2I,x1M)),
		Rule(248, FValue(0.5, Token(248)), insvIM(x0I,x4M),CICM(x3C,x2I,x4M),insvIM(x2I,x1M),virtIM(x0I,x1M)),
		Rule(249, FValue(0.5, Token(249)), insvIM(x0I,x4M),CICM(x3C,x2I,x4M),virtIM(x0I,x1M),virtIM(x2I,x1M)),
		Rule(250, FValue(0.5, Token(250)), insvIM(x4I,x1M),CICM(x2C,x0I,x3M),insvIM(x0I,x1M),insvIM(x4I,x3M)),
		Rule(251, FValue(0.5, Token(251)), insvIM(x4I,x1M),CICM(x2C,x0I,x3M),insvIM(x0I,x1M),virtIM(x4I,x3M)),
		Rule(252, FValue(0.5, Token(252)), insvIM(x4I,x1M),CICM(x2C,x0I,x3M),insvIM(x4I,x3M),virtIM(x0I,x1M)),
		Rule(253, FValue(0.5, Token(253)), insvIM(x4I,x1M),CICM(x2C,x0I,x3M),virtIM(x0I,x1M),virtIM(x4I,x3M)),
		Rule(254, FValue(0.5, Token(254)), insvIM(x4I,x1M),CICM(x2C,x0I,x3M),CICM(x2C,x4I,x5M),insvIM(x0I,x1M)),
		Rule(255, FValue(0.5, Token(255)), insvIM(x4I,x1M),CICM(x2C,x0I,x3M),CICM(x2C,x4I,x5M),virtIM(x0I,x1M)),
		Rule(256, FValue(0.5, Token(256)), insvIM(x5I,x1M),CICM(x2C,x0I,x3M),CICM(x4C,x5I,x3M),insvIM(x0I,x1M)),
		Rule(257, FValue(0.5, Token(257)), insvIM(x5I,x1M),CICM(x2C,x0I,x3M),CICM(x4C,x5I,x3M),virtIM(x0I,x1M)),
		Rule(258, FValue(0.5, Token(258)), insvIM(x0I,x5M),CICM(x2C,x3I,x1M),CICM(x4C,x3I,x5M),insvIM(x0I,x1M)),
		Rule(259, FValue(0.5, Token(259)), insvIM(x0I,x5M),CICM(x2C,x3I,x1M),CICM(x4C,x3I,x5M),virtIM(x0I,x1M)),
		Rule(260, FValue(0.5, Token(260)), insvIM(x1I,x3M),CICM(x0C,x1I,x2M),CICM(x4C,x5I,x3M),Mneq(x2M,x3M)),
		Rule(261, FValue(0.5, Token(261)), insvIM(x3I,x2M),CICM(x0C,x1I,x2M),virtI(x3I),virtIM(x3I,x2M)),
		Rule(262, FValue(0.5, Token(262)), insvIM(x3I,x2M),CICM(x0C,x1I,x2M),polySite(x3I),virtIM(x3I,x2M)),
		Rule(263, FValue(0.5, Token(263)), insvIM(x3I,x0M),Mneq(x0M,x2M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
		Rule(264, FValue(0.5, Token(264)), insvIM(x3I,x0M),Mneq(x0M,x2M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
		Rule(265, FValue(0.5, Token(265)), insvIM(x3I,x1M),Mneq(x1M,x2M),insvIM(x3I,x2M),virtIM(x3I,x1M)),
		Rule(266, FValue(0.5, Token(266)), insvIM(x3I,x1M),Mneq(x1M,x2M),virtIM(x3I,x1M),virtIM(x3I,x2M)),
		Rule(267, FValue(0.5, Token(267)), insvIM(x3I,x1M),Mneq(x2M,x1M),insvIM(x3I,x2M),virtIM(x3I,x1M)),
		Rule(268, FValue(0.5, Token(268)), insvIM(x3I,x1M),Mneq(x2M,x1M),virtIM(x3I,x1M),virtIM(x3I,x2M)),
		Rule(269, FValue(0.5, Token(269)), insvIM(x4I,x0M),CICM(x3C,x4I,x2M),Mneq(x0M,x2M),Mneq(x2M,x0M)),
		Rule(270, FValue(0.5, Token(270)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x4I,x1M)),
		Rule(271, FValue(0.5, Token(271)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x4I,x1M)),
		Rule(272, FValue(0.5, Token(272)), insvIM(x2I,x1M),CICM(x3C,x2I,x0M),Mneq(x0M,x1M),insvIM(x2I,x0M)),
		Rule(273, FValue(0.5, Token(273)), insvIM(x2I,x1M),CICM(x3C,x2I,x0M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
		Rule(274, FValue(0.5, Token(274)), insvIM(x2I,x1M),CICM(x3C,x2I,x1M),Mneq(x0M,x1M),insvIM(x2I,x0M)),
		Rule(275, FValue(0.5, Token(275)), insvIM(x2I,x1M),CICM(x3C,x2I,x1M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
		Rule(276, FValue(0.5, Token(276)), insvIM(x2I,x0M),CICM(x3C,x2I,x0M),Mneq(x0M,x1M),insvIM(x2I,x1M)),
		Rule(277, FValue(0.5, Token(277)), insvIM(x2I,x0M),CICM(x3C,x2I,x0M),Mneq(x0M,x1M),virtIM(x2I,x1M)),
		Rule(278, FValue(0.5, Token(278)), insvIM(x4I,x1M),CICM(x2C,x3I,x0M),CICM(x2C,x4I,x1M),Mneq(x0M,x1M)),
		Rule(279, FValue(0.5, Token(279)), insvIM(x3I,x1M),CICM(x2C,x3I,x0M),CICM(x4C,x3I,x1M),Mneq(x0M,x1M)),
		Rule(280, FValue(0.5, Token(280)), insvIM(x4I,x1M),CICM(x2C,x3I,x1M),CICM(x2C,x4I,x5M),insvIM(x3I,x1M)),
		Rule(281, FValue(0.5, Token(281)), insvIM(x4I,x1M),CICM(x2C,x3I,x1M),CICM(x2C,x4I,x5M),virtIM(x3I,x1M)),
		Rule(282, FValue(0.5, Token(282)), insvIM(x4I,x1M),CICM(x2C,x3I,x1M),CICM(x2C,x4I,x5M),Mneq(x5M,x1M)),
		Rule(283, FValue(0.5, Token(283)), insvIM(x4I,x1M),CICM(x2C,x0I,x5M),CICM(x2C,x4I,x5M),insvIM(x0I,x1M)),
		Rule(284, FValue(0.5, Token(284)), insvIM(x4I,x1M),CICM(x2C,x0I,x5M),CICM(x2C,x4I,x5M),virtIM(x0I,x1M)),
		Rule(285, FValue(0.5, Token(285)), insvIM(x4I,x1M),CICM(x2C,x0I,x3M),CICM(x2C,x4I,x1M),insvIM(x0I,x1M)),
		Rule(286, FValue(0.5, Token(286)), insvIM(x4I,x1M),CICM(x2C,x0I,x3M),CICM(x2C,x4I,x1M),virtIM(x0I,x1M)),
		Rule(287, FValue(0.5, Token(287)), insvIM(x0I,x5M),CICM(x2C,x4I,x1M),CICM(x2C,x4I,x5M),insvIM(x0I,x1M)),
		Rule(288, FValue(0.5, Token(288)), insvIM(x0I,x5M),CICM(x2C,x4I,x1M),CICM(x2C,x4I,x5M),virtIM(x0I,x1M)),
	)
	val soupProg = Program("polysiteSoup", soup)
	val evaluator = SeminaiveEvaluator(soupProg)

	override val expected: Set[Any] = Set(2, 139, 160)
	override val maxVarCount: Int = 3
}
