package qd
package learner
import org.scalatest.{FunSuite, Ignore}

class Gensamegen_1 extends Problem {
	override val name = "samegen"
	val VSet = Range(0, 10).map(i => Atom(i)).toSet
	val V = Domain("V", VSet)
	val parent = Relation("parent", V,V)
	val sgen = Relation("sgen", V,V)
	val parentTuples = Set((2, 1),(3, 1),(4, 2),(5, 2),(6, 3),(7, 3),(8, 9)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val sgenTuples = Set((2, 2),(3, 3),(4, 4),(5, 5),(6, 6),(7, 7),(2, 3),(3, 2),(4, 5),(4, 6),(4, 7),(5, 4),(5, 6),(5, 7),(6, 4),(6, 5),(6, 7),(7, 4),(7, 5),(7, 6),(8, 8),(7, 2)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
override val edb = Config(
parent -> (Instance[FValue](parent) ++ parentTuples.map(t => t -> FValue.One).toMap),
)
override val refOut = Config(
sgen -> (Instance[FValue](sgen) ++ sgenTuples.map(t => t -> FValue.One).toMap),
)
	val x1V = Variable("x1V",V)
	val x3V = Variable("x3V",V)
	val x0V = Variable("x0V",V)
	val x2V = Variable("x2V",V)
	val soup = Set(
		Rule(0, FValue(0.5, Token(0)), sgen(x0V,x1V),parent(x0V,x1V),sgen(x0V,x2V)),
		Rule(1, FValue(0.5, Token(1)), sgen(x1V,x2V),sgen(x0V,x1V),sgen(x0V,x2V)),
		Rule(2, FValue(0.5, Token(2)), sgen(x1V,x2V),parent(x0V,x2V),sgen(x0V,x1V)),
		Rule(3, FValue(0.5, Token(3)), sgen(x1V,x2V),parent(x0V,x1V),sgen(x0V,x2V)),
		Rule(4, FValue(0.5, Token(4)), sgen(x1V,x2V),parent(x0V,x1V),parent(x0V,x2V)),
		Rule(5, FValue(0.5, Token(5)), sgen(x0V,x1V),parent(x0V,x1V),sgen(x2V,x0V)),
		Rule(6, FValue(0.5, Token(6)), sgen(x0V,x1V),parent(x0V,x1V),parent(x2V,x0V)),
		Rule(7, FValue(0.5, Token(7)), sgen(x0V,x2V),sgen(x0V,x1V),sgen(x2V,x0V)),
		Rule(8, FValue(0.5, Token(8)), sgen(x0V,x2V),parent(x2V,x0V),sgen(x0V,x1V)),
		Rule(9, FValue(0.5, Token(9)), sgen(x0V,x2V),parent(x0V,x1V),sgen(x2V,x0V)),
		Rule(10, FValue(0.5, Token(10)), sgen(x0V,x2V),parent(x0V,x1V),parent(x2V,x0V)),
		Rule(11, FValue(0.5, Token(11)), sgen(x1V,x0V),sgen(x0V,x1V),sgen(x2V,x0V)),
		Rule(12, FValue(0.5, Token(12)), sgen(x1V,x0V),parent(x2V,x0V),sgen(x0V,x1V)),
		Rule(13, FValue(0.5, Token(13)), sgen(x1V,x0V),parent(x0V,x1V),sgen(x2V,x0V)),
		Rule(14, FValue(0.5, Token(14)), sgen(x1V,x0V),parent(x0V,x1V),parent(x2V,x0V)),
		Rule(15, FValue(0.5, Token(15)), sgen(x1V,x2V),sgen(x0V,x1V),sgen(x2V,x0V)),
		Rule(16, FValue(0.5, Token(16)), sgen(x1V,x2V),parent(x2V,x0V),sgen(x0V,x1V)),
		Rule(17, FValue(0.5, Token(17)), sgen(x1V,x2V),parent(x0V,x1V),sgen(x2V,x0V)),
		Rule(18, FValue(0.5, Token(18)), sgen(x1V,x2V),parent(x0V,x1V),parent(x2V,x0V)),
		Rule(19, FValue(0.5, Token(19)), sgen(x2V,x0V),parent(x2V,x0V),sgen(x0V,x1V)),
		Rule(20, FValue(0.5, Token(20)), sgen(x2V,x0V),parent(x0V,x1V),parent(x2V,x0V)),
		Rule(21, FValue(0.5, Token(21)), sgen(x2V,x1V),sgen(x0V,x1V),sgen(x2V,x0V)),
		Rule(22, FValue(0.5, Token(22)), sgen(x2V,x1V),parent(x2V,x0V),sgen(x0V,x1V)),
		Rule(23, FValue(0.5, Token(23)), sgen(x2V,x1V),parent(x0V,x1V),sgen(x2V,x0V)),
		Rule(24, FValue(0.5, Token(24)), sgen(x2V,x1V),parent(x0V,x1V),parent(x2V,x0V)),
		Rule(25, FValue(0.5, Token(25)), sgen(x0V,x2V),sgen(x0V,x1V),sgen(x2V,x1V)),
		Rule(26, FValue(0.5, Token(26)), sgen(x0V,x2V),parent(x2V,x1V),sgen(x0V,x1V)),
		Rule(27, FValue(0.5, Token(27)), sgen(x0V,x2V),parent(x0V,x1V),sgen(x2V,x1V)),
		Rule(28, FValue(0.5, Token(28)), sgen(x0V,x2V),parent(x0V,x1V),parent(x2V,x1V)),
		Rule(29, FValue(0.5, Token(29)), sgen(x0V,x2V),parent(x0V,x2V),sgen(x0V,x1V),sgen(x2V,x3V)),
		Rule(30, FValue(0.5, Token(30)), sgen(x0V,x2V),parent(x0V,x2V),parent(x2V,x3V),sgen(x0V,x1V)),
		Rule(31, FValue(0.5, Token(31)), sgen(x0V,x3V),parent(x0V,x2V),sgen(x0V,x1V),sgen(x2V,x3V)),
		Rule(32, FValue(0.5, Token(32)), sgen(x0V,x3V),parent(x0V,x2V),parent(x2V,x3V),sgen(x0V,x1V)),
		Rule(33, FValue(0.5, Token(33)), sgen(x0V,x3V),parent(x0V,x1V),sgen(x0V,x2V),sgen(x2V,x3V)),
		Rule(34, FValue(0.5, Token(34)), sgen(x0V,x3V),parent(x0V,x1V),parent(x2V,x3V),sgen(x0V,x2V)),
		Rule(35, FValue(0.5, Token(35)), sgen(x1V,x3V),sgen(x0V,x1V),sgen(x0V,x2V),sgen(x2V,x3V)),
		Rule(36, FValue(0.5, Token(36)), sgen(x1V,x3V),parent(x2V,x3V),sgen(x0V,x1V),sgen(x0V,x2V)),
		Rule(37, FValue(0.5, Token(37)), sgen(x1V,x3V),parent(x0V,x2V),sgen(x0V,x1V),sgen(x2V,x3V)),
		Rule(38, FValue(0.5, Token(38)), sgen(x1V,x3V),parent(x0V,x2V),parent(x2V,x3V),sgen(x0V,x1V)),
		Rule(39, FValue(0.5, Token(39)), sgen(x1V,x3V),parent(x0V,x1V),sgen(x0V,x2V),sgen(x2V,x3V)),
		Rule(40, FValue(0.5, Token(40)), sgen(x1V,x3V),parent(x0V,x1V),parent(x2V,x3V),sgen(x0V,x2V)),
		Rule(41, FValue(0.5, Token(41)), sgen(x1V,x3V),parent(x0V,x1V),parent(x0V,x2V),sgen(x2V,x3V)),
		Rule(42, FValue(0.5, Token(42)), sgen(x1V,x3V),parent(x0V,x1V),parent(x0V,x2V),parent(x2V,x3V)),
		Rule(43, FValue(0.5, Token(43)), sgen(x2V,x1V),sgen(x0V,x1V),sgen(x0V,x2V),sgen(x2V,x3V)),
		Rule(44, FValue(0.5, Token(44)), sgen(x2V,x1V),parent(x2V,x3V),sgen(x0V,x1V),sgen(x0V,x2V)),
		Rule(45, FValue(0.5, Token(45)), sgen(x2V,x1V),parent(x0V,x2V),sgen(x0V,x1V),sgen(x2V,x3V)),
		Rule(46, FValue(0.5, Token(46)), sgen(x2V,x1V),parent(x0V,x2V),parent(x2V,x3V),sgen(x0V,x1V)),
		Rule(47, FValue(0.5, Token(47)), sgen(x2V,x1V),parent(x0V,x1V),sgen(x0V,x2V),sgen(x2V,x3V)),
		Rule(48, FValue(0.5, Token(48)), sgen(x2V,x1V),parent(x0V,x1V),parent(x2V,x3V),sgen(x0V,x2V)),
		Rule(49, FValue(0.5, Token(49)), sgen(x2V,x1V),parent(x0V,x1V),parent(x0V,x2V),sgen(x2V,x3V)),
		Rule(50, FValue(0.5, Token(50)), sgen(x2V,x1V),parent(x0V,x1V),parent(x0V,x2V),parent(x2V,x3V)),
		Rule(51, FValue(0.5, Token(51)), sgen(x3V,x1V),sgen(x0V,x1V),sgen(x0V,x2V),sgen(x2V,x3V)),
		Rule(52, FValue(0.5, Token(52)), sgen(x3V,x1V),parent(x2V,x3V),sgen(x0V,x1V),sgen(x0V,x2V)),
		Rule(53, FValue(0.5, Token(53)), sgen(x3V,x1V),parent(x0V,x2V),sgen(x0V,x1V),sgen(x2V,x3V)),
		Rule(54, FValue(0.5, Token(54)), sgen(x3V,x1V),parent(x0V,x2V),parent(x2V,x3V),sgen(x0V,x1V)),
		Rule(55, FValue(0.5, Token(55)), sgen(x3V,x1V),parent(x0V,x1V),sgen(x0V,x2V),sgen(x2V,x3V)),
		Rule(56, FValue(0.5, Token(56)), sgen(x3V,x1V),parent(x0V,x1V),parent(x2V,x3V),sgen(x0V,x2V)),
		Rule(57, FValue(0.5, Token(57)), sgen(x3V,x1V),parent(x0V,x1V),parent(x0V,x2V),sgen(x2V,x3V)),
		Rule(58, FValue(0.5, Token(58)), sgen(x3V,x1V),parent(x0V,x1V),parent(x0V,x2V),parent(x2V,x3V)),
		Rule(59, FValue(0.5, Token(59)), sgen(x0V,x2V),parent(x0V,x2V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(60, FValue(0.5, Token(60)), sgen(x3V,x1V),sgen(x0V,x1V),sgen(x0V,x2V),sgen(x3V,x2V)),
		Rule(61, FValue(0.5, Token(61)), sgen(x3V,x1V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x0V,x2V)),
		Rule(62, FValue(0.5, Token(62)), sgen(x3V,x1V),parent(x0V,x2V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(63, FValue(0.5, Token(63)), sgen(x3V,x1V),parent(x0V,x2V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(64, FValue(0.5, Token(64)), sgen(x3V,x1V),parent(x0V,x1V),sgen(x0V,x2V),sgen(x3V,x2V)),
		Rule(65, FValue(0.5, Token(65)), sgen(x3V,x1V),parent(x0V,x1V),parent(x3V,x2V),sgen(x0V,x2V)),
		Rule(66, FValue(0.5, Token(66)), sgen(x3V,x1V),parent(x0V,x1V),parent(x0V,x2V),sgen(x3V,x2V)),
		Rule(67, FValue(0.5, Token(67)), sgen(x3V,x1V),parent(x0V,x1V),parent(x0V,x2V),parent(x3V,x2V)),
		Rule(68, FValue(0.5, Token(68)), sgen(x0V,x2V),sgen(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(69, FValue(0.5, Token(69)), sgen(x0V,x2V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x2V,x0V)),
		Rule(70, FValue(0.5, Token(70)), sgen(x0V,x2V),parent(x2V,x0V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(71, FValue(0.5, Token(71)), sgen(x0V,x2V),parent(x2V,x0V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(72, FValue(0.5, Token(72)), sgen(x0V,x2V),parent(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(73, FValue(0.5, Token(73)), sgen(x0V,x2V),parent(x0V,x1V),parent(x3V,x2V),sgen(x2V,x0V)),
		Rule(74, FValue(0.5, Token(74)), sgen(x0V,x2V),parent(x0V,x1V),parent(x2V,x0V),sgen(x3V,x2V)),
		Rule(75, FValue(0.5, Token(75)), sgen(x0V,x2V),parent(x0V,x1V),parent(x2V,x0V),parent(x3V,x2V)),
		Rule(76, FValue(0.5, Token(76)), sgen(x0V,x3V),sgen(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(77, FValue(0.5, Token(77)), sgen(x0V,x3V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x2V,x0V)),
		Rule(78, FValue(0.5, Token(78)), sgen(x0V,x3V),parent(x2V,x0V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(79, FValue(0.5, Token(79)), sgen(x0V,x3V),parent(x2V,x0V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(80, FValue(0.5, Token(80)), sgen(x0V,x3V),parent(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(81, FValue(0.5, Token(81)), sgen(x0V,x3V),parent(x0V,x1V),parent(x3V,x2V),sgen(x2V,x0V)),
		Rule(82, FValue(0.5, Token(82)), sgen(x0V,x3V),parent(x0V,x1V),parent(x2V,x0V),sgen(x3V,x2V)),
		Rule(83, FValue(0.5, Token(83)), sgen(x0V,x3V),parent(x0V,x1V),parent(x2V,x0V),parent(x3V,x2V)),
		Rule(84, FValue(0.5, Token(84)), sgen(x1V,x2V),sgen(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(85, FValue(0.5, Token(85)), sgen(x1V,x2V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x2V,x0V)),
		Rule(86, FValue(0.5, Token(86)), sgen(x1V,x2V),parent(x2V,x0V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(87, FValue(0.5, Token(87)), sgen(x1V,x2V),parent(x2V,x0V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(88, FValue(0.5, Token(88)), sgen(x1V,x2V),parent(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(89, FValue(0.5, Token(89)), sgen(x1V,x2V),parent(x0V,x1V),parent(x3V,x2V),sgen(x2V,x0V)),
		Rule(90, FValue(0.5, Token(90)), sgen(x1V,x2V),parent(x0V,x1V),parent(x2V,x0V),sgen(x3V,x2V)),
		Rule(91, FValue(0.5, Token(91)), sgen(x1V,x2V),parent(x0V,x1V),parent(x2V,x0V),parent(x3V,x2V)),
		Rule(92, FValue(0.5, Token(92)), sgen(x1V,x3V),sgen(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(93, FValue(0.5, Token(93)), sgen(x1V,x3V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x2V,x0V)),
		Rule(94, FValue(0.5, Token(94)), sgen(x1V,x3V),parent(x2V,x0V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(95, FValue(0.5, Token(95)), sgen(x1V,x3V),parent(x2V,x0V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(96, FValue(0.5, Token(96)), sgen(x1V,x3V),parent(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(97, FValue(0.5, Token(97)), sgen(x1V,x3V),parent(x0V,x1V),parent(x3V,x2V),sgen(x2V,x0V)),
		Rule(98, FValue(0.5, Token(98)), sgen(x1V,x3V),parent(x0V,x1V),parent(x2V,x0V),sgen(x3V,x2V)),
		Rule(99, FValue(0.5, Token(99)), sgen(x1V,x3V),parent(x0V,x1V),parent(x2V,x0V),parent(x3V,x2V)),
		Rule(100, FValue(0.5, Token(100)), sgen(x2V,x0V),parent(x2V,x0V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(101, FValue(0.5, Token(101)), sgen(x2V,x0V),parent(x2V,x0V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(102, FValue(0.5, Token(102)), sgen(x2V,x0V),parent(x0V,x1V),parent(x2V,x0V),sgen(x3V,x2V)),
		Rule(103, FValue(0.5, Token(103)), sgen(x2V,x0V),parent(x0V,x1V),parent(x2V,x0V),parent(x3V,x2V)),
		Rule(104, FValue(0.5, Token(104)), sgen(x2V,x1V),sgen(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(105, FValue(0.5, Token(105)), sgen(x2V,x1V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x2V,x0V)),
		Rule(106, FValue(0.5, Token(106)), sgen(x2V,x1V),parent(x2V,x0V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(107, FValue(0.5, Token(107)), sgen(x2V,x1V),parent(x2V,x0V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(108, FValue(0.5, Token(108)), sgen(x2V,x1V),parent(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(109, FValue(0.5, Token(109)), sgen(x2V,x1V),parent(x0V,x1V),parent(x3V,x2V),sgen(x2V,x0V)),
		Rule(110, FValue(0.5, Token(110)), sgen(x2V,x1V),parent(x0V,x1V),parent(x2V,x0V),sgen(x3V,x2V)),
		Rule(111, FValue(0.5, Token(111)), sgen(x2V,x1V),parent(x0V,x1V),parent(x2V,x0V),parent(x3V,x2V)),
		Rule(112, FValue(0.5, Token(112)), sgen(x3V,x0V),sgen(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(113, FValue(0.5, Token(113)), sgen(x3V,x0V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x2V,x0V)),
		Rule(114, FValue(0.5, Token(114)), sgen(x3V,x0V),parent(x2V,x0V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(115, FValue(0.5, Token(115)), sgen(x3V,x0V),parent(x2V,x0V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(116, FValue(0.5, Token(116)), sgen(x3V,x0V),parent(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(117, FValue(0.5, Token(117)), sgen(x3V,x0V),parent(x0V,x1V),parent(x3V,x2V),sgen(x2V,x0V)),
		Rule(118, FValue(0.5, Token(118)), sgen(x3V,x0V),parent(x0V,x1V),parent(x2V,x0V),sgen(x3V,x2V)),
		Rule(119, FValue(0.5, Token(119)), sgen(x3V,x0V),parent(x0V,x1V),parent(x2V,x0V),parent(x3V,x2V)),
		Rule(120, FValue(0.5, Token(120)), sgen(x3V,x1V),sgen(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(121, FValue(0.5, Token(121)), sgen(x3V,x1V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x2V,x0V)),
		Rule(122, FValue(0.5, Token(122)), sgen(x3V,x1V),parent(x2V,x0V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(123, FValue(0.5, Token(123)), sgen(x3V,x1V),parent(x2V,x0V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(124, FValue(0.5, Token(124)), sgen(x3V,x1V),parent(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(125, FValue(0.5, Token(125)), sgen(x3V,x1V),parent(x0V,x1V),parent(x3V,x2V),sgen(x2V,x0V)),
		Rule(126, FValue(0.5, Token(126)), sgen(x3V,x1V),parent(x0V,x1V),parent(x2V,x0V),sgen(x3V,x2V)),
		Rule(127, FValue(0.5, Token(127)), sgen(x3V,x1V),parent(x0V,x1V),parent(x2V,x0V),parent(x3V,x2V)),
		Rule(128, FValue(0.5, Token(128)), sgen(x0V,x3V),sgen(x0V,x1V),sgen(x1V,x2V),sgen(x3V,x2V)),
		Rule(129, FValue(0.5, Token(129)), sgen(x0V,x3V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x1V,x2V)),
		Rule(130, FValue(0.5, Token(130)), sgen(x0V,x3V),parent(x1V,x2V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(131, FValue(0.5, Token(131)), sgen(x0V,x3V),parent(x1V,x2V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(132, FValue(0.5, Token(132)), sgen(x0V,x3V),parent(x0V,x1V),sgen(x1V,x2V),sgen(x3V,x2V)),
		Rule(133, FValue(0.5, Token(133)), sgen(x0V,x3V),parent(x0V,x1V),parent(x3V,x2V),sgen(x1V,x2V)),
		Rule(134, FValue(0.5, Token(134)), sgen(x0V,x3V),parent(x0V,x1V),parent(x1V,x2V),sgen(x3V,x2V)),
		Rule(135, FValue(0.5, Token(135)), sgen(x0V,x3V),parent(x0V,x1V),parent(x1V,x2V),parent(x3V,x2V)),
		Rule(136, FValue(0.5, Token(136)), sgen(x1V,x2V),parent(x1V,x2V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(137, FValue(0.5, Token(137)), sgen(x1V,x2V),parent(x0V,x1V),parent(x1V,x2V),sgen(x3V,x2V)),
		Rule(138, FValue(0.5, Token(138)), sgen(x3V,x0V),sgen(x0V,x1V),sgen(x1V,x2V),sgen(x3V,x2V)),
		Rule(139, FValue(0.5, Token(139)), sgen(x3V,x0V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x1V,x2V)),
		Rule(140, FValue(0.5, Token(140)), sgen(x3V,x0V),parent(x1V,x2V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(141, FValue(0.5, Token(141)), sgen(x3V,x0V),parent(x1V,x2V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(142, FValue(0.5, Token(142)), sgen(x3V,x0V),parent(x0V,x1V),sgen(x1V,x2V),sgen(x3V,x2V)),
		Rule(143, FValue(0.5, Token(143)), sgen(x3V,x0V),parent(x0V,x1V),parent(x3V,x2V),sgen(x1V,x2V)),
		Rule(144, FValue(0.5, Token(144)), sgen(x3V,x0V),parent(x0V,x1V),parent(x1V,x2V),sgen(x3V,x2V)),
		Rule(145, FValue(0.5, Token(145)), sgen(x3V,x0V),parent(x0V,x1V),parent(x1V,x2V),parent(x3V,x2V)),
		Rule(146, FValue(0.5, Token(146)), sgen(x3V,x1V),sgen(x0V,x1V),sgen(x1V,x2V),sgen(x3V,x2V)),
		Rule(147, FValue(0.5, Token(147)), sgen(x3V,x1V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x1V,x2V)),
		Rule(148, FValue(0.5, Token(148)), sgen(x3V,x1V),parent(x1V,x2V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(149, FValue(0.5, Token(149)), sgen(x3V,x1V),parent(x1V,x2V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(150, FValue(0.5, Token(150)), sgen(x3V,x1V),parent(x0V,x1V),sgen(x1V,x2V),sgen(x3V,x2V)),
		Rule(151, FValue(0.5, Token(151)), sgen(x3V,x1V),parent(x0V,x1V),parent(x3V,x2V),sgen(x1V,x2V)),
		Rule(152, FValue(0.5, Token(152)), sgen(x3V,x1V),parent(x0V,x1V),parent(x1V,x2V),sgen(x3V,x2V)),
		Rule(153, FValue(0.5, Token(153)), sgen(x3V,x1V),parent(x0V,x1V),parent(x1V,x2V),parent(x3V,x2V)),
	)
	val soupProg = Program("samegenSoup", soup)
	val evaluator = SeminaiveEvaluator(soupProg)

	override val expected = Set(28, 133)
	override val maxVarCount: Int = 20
}
