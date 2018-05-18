package qd
package learner
import org.scalatest.{FunSuite, Ignore}

class SamegenNLP extends FunSuite {
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
  val soup = Set(
		Rule(1, Value(0.500000, Token(1)), sgen(x0V,x1V),parent(x0V,x1V),sgen(x0V,x2V)),
		Rule(2, Value(0.500000, Token(2)), sgen(x1V,x2V),sgen(x0V,x1V),sgen(x0V,x2V)),
		Rule(3, Value(0.500000, Token(3)), sgen(x1V,x2V),parent(x0V,x2V),sgen(x0V,x1V)),
		Rule(4, Value(0.500000, Token(4)), sgen(x1V,x2V),parent(x0V,x1V),sgen(x0V,x2V)),
		Rule(5, Value(0.500000, Token(5)), sgen(x1V,x2V),parent(x0V,x1V),parent(x0V,x2V)),
		Rule(6, Value(0.500000, Token(6)), sgen(x0V,x1V),parent(x0V,x1V),sgen(x2V,x0V)),
		Rule(7, Value(0.500000, Token(7)), sgen(x0V,x1V),parent(x0V,x1V),parent(x2V,x0V)),
		Rule(8, Value(0.500000, Token(8)), sgen(x0V,x2V),sgen(x0V,x1V),sgen(x2V,x0V)),
		Rule(9, Value(0.500000, Token(9)), sgen(x0V,x2V),parent(x2V,x0V),sgen(x0V,x1V)),
		Rule(10, Value(0.500000, Token(10)), sgen(x0V,x2V),parent(x0V,x1V),sgen(x2V,x0V)),
		Rule(11, Value(0.500000, Token(11)), sgen(x0V,x2V),parent(x0V,x1V),parent(x2V,x0V)),
		Rule(12, Value(0.500000, Token(12)), sgen(x1V,x0V),sgen(x0V,x1V),sgen(x2V,x0V)),
		Rule(13, Value(0.500000, Token(13)), sgen(x1V,x0V),parent(x2V,x0V),sgen(x0V,x1V)),
		Rule(14, Value(0.500000, Token(14)), sgen(x1V,x0V),parent(x0V,x1V),sgen(x2V,x0V)),
		Rule(15, Value(0.500000, Token(15)), sgen(x1V,x0V),parent(x0V,x1V),parent(x2V,x0V)),
		Rule(16, Value(0.500000, Token(16)), sgen(x1V,x2V),sgen(x0V,x1V),sgen(x2V,x0V)),
		Rule(17, Value(0.500000, Token(17)), sgen(x1V,x2V),parent(x2V,x0V),sgen(x0V,x1V)),
		Rule(18, Value(0.500000, Token(18)), sgen(x1V,x2V),parent(x0V,x1V),sgen(x2V,x0V)),
		Rule(19, Value(0.500000, Token(19)), sgen(x1V,x2V),parent(x0V,x1V),parent(x2V,x0V)),
		Rule(20, Value(0.500000, Token(20)), sgen(x2V,x0V),parent(x2V,x0V),sgen(x0V,x1V)),
		Rule(21, Value(0.500000, Token(21)), sgen(x2V,x0V),parent(x0V,x1V),parent(x2V,x0V)),
		Rule(22, Value(0.500000, Token(22)), sgen(x2V,x1V),sgen(x0V,x1V),sgen(x2V,x0V)),
		Rule(23, Value(0.500000, Token(23)), sgen(x2V,x1V),parent(x2V,x0V),sgen(x0V,x1V)),
		Rule(24, Value(0.500000, Token(24)), sgen(x2V,x1V),parent(x0V,x1V),sgen(x2V,x0V)),
		Rule(25, Value(0.500000, Token(25)), sgen(x2V,x1V),parent(x0V,x1V),parent(x2V,x0V)),
		Rule(26, Value(0.500000, Token(26)), sgen(x0V,x2V),sgen(x0V,x1V),sgen(x2V,x1V)),
		Rule(27, Value(0.500000, Token(27)), sgen(x0V,x2V),parent(x2V,x1V),sgen(x0V,x1V)),
		Rule(28, Value(0.500000, Token(28)), sgen(x0V,x2V),parent(x0V,x1V),sgen(x2V,x1V)),
		Rule(29, Value(0.500000, Token(29)), sgen(x0V,x2V),parent(x0V,x1V),parent(x2V,x1V)),
		Rule(30, Value(0.500000, Token(30)), sgen(x0V,x2V),parent(x0V,x2V),sgen(x0V,x1V),sgen(x2V,x3V)),
		Rule(31, Value(0.500000, Token(31)), sgen(x0V,x2V),parent(x0V,x2V),parent(x2V,x3V),sgen(x0V,x1V)),
		Rule(32, Value(0.500000, Token(32)), sgen(x0V,x3V),parent(x0V,x2V),sgen(x0V,x1V),sgen(x2V,x3V)),
		Rule(33, Value(0.500000, Token(33)), sgen(x0V,x3V),parent(x0V,x2V),parent(x2V,x3V),sgen(x0V,x1V)),
		Rule(34, Value(0.500000, Token(34)), sgen(x0V,x3V),parent(x0V,x1V),sgen(x0V,x2V),sgen(x2V,x3V)),
		Rule(35, Value(0.500000, Token(35)), sgen(x0V,x3V),parent(x0V,x1V),parent(x2V,x3V),sgen(x0V,x2V)),
		Rule(36, Value(0.500000, Token(36)), sgen(x1V,x3V),sgen(x0V,x1V),sgen(x0V,x2V),sgen(x2V,x3V)),
		Rule(37, Value(0.500000, Token(37)), sgen(x1V,x3V),parent(x2V,x3V),sgen(x0V,x1V),sgen(x0V,x2V)),
		Rule(38, Value(0.500000, Token(38)), sgen(x1V,x3V),parent(x0V,x2V),sgen(x0V,x1V),sgen(x2V,x3V)),
		Rule(39, Value(0.500000, Token(39)), sgen(x1V,x3V),parent(x0V,x2V),parent(x2V,x3V),sgen(x0V,x1V)),
		Rule(40, Value(0.500000, Token(40)), sgen(x1V,x3V),parent(x0V,x1V),sgen(x0V,x2V),sgen(x2V,x3V)),
		Rule(41, Value(0.500000, Token(41)), sgen(x1V,x3V),parent(x0V,x1V),parent(x2V,x3V),sgen(x0V,x2V)),
		Rule(42, Value(0.500000, Token(42)), sgen(x1V,x3V),parent(x0V,x1V),parent(x0V,x2V),sgen(x2V,x3V)),
		Rule(43, Value(0.500000, Token(43)), sgen(x1V,x3V),parent(x0V,x1V),parent(x0V,x2V),parent(x2V,x3V)),
		Rule(44, Value(0.500000, Token(44)), sgen(x2V,x1V),sgen(x0V,x1V),sgen(x0V,x2V),sgen(x2V,x3V)),
		Rule(45, Value(0.500000, Token(45)), sgen(x2V,x1V),parent(x2V,x3V),sgen(x0V,x1V),sgen(x0V,x2V)),
		Rule(46, Value(0.500000, Token(46)), sgen(x2V,x1V),parent(x0V,x2V),sgen(x0V,x1V),sgen(x2V,x3V)),
		Rule(47, Value(0.500000, Token(47)), sgen(x2V,x1V),parent(x0V,x2V),parent(x2V,x3V),sgen(x0V,x1V)),
		Rule(48, Value(0.500000, Token(48)), sgen(x2V,x1V),parent(x0V,x1V),sgen(x0V,x2V),sgen(x2V,x3V)),
		Rule(49, Value(0.500000, Token(49)), sgen(x2V,x1V),parent(x0V,x1V),parent(x2V,x3V),sgen(x0V,x2V)),
		Rule(50, Value(0.500000, Token(50)), sgen(x2V,x1V),parent(x0V,x1V),parent(x0V,x2V),sgen(x2V,x3V)),
		Rule(51, Value(0.500000, Token(51)), sgen(x2V,x1V),parent(x0V,x1V),parent(x0V,x2V),parent(x2V,x3V)),
		Rule(52, Value(0.500000, Token(52)), sgen(x3V,x1V),sgen(x0V,x1V),sgen(x0V,x2V),sgen(x2V,x3V)),
		Rule(53, Value(0.500000, Token(53)), sgen(x3V,x1V),parent(x2V,x3V),sgen(x0V,x1V),sgen(x0V,x2V)),
		Rule(54, Value(0.500000, Token(54)), sgen(x3V,x1V),parent(x0V,x2V),sgen(x0V,x1V),sgen(x2V,x3V)),
		Rule(55, Value(0.500000, Token(55)), sgen(x3V,x1V),parent(x0V,x2V),parent(x2V,x3V),sgen(x0V,x1V)),
		Rule(56, Value(0.500000, Token(56)), sgen(x3V,x1V),parent(x0V,x1V),sgen(x0V,x2V),sgen(x2V,x3V)),
		Rule(57, Value(0.500000, Token(57)), sgen(x3V,x1V),parent(x0V,x1V),parent(x2V,x3V),sgen(x0V,x2V)),
		Rule(58, Value(0.500000, Token(58)), sgen(x3V,x1V),parent(x0V,x1V),parent(x0V,x2V),sgen(x2V,x3V)),
		Rule(59, Value(0.500000, Token(59)), sgen(x3V,x1V),parent(x0V,x1V),parent(x0V,x2V),parent(x2V,x3V)),
		Rule(60, Value(0.500000, Token(60)), sgen(x0V,x2V),parent(x0V,x2V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(61, Value(0.500000, Token(61)), sgen(x3V,x1V),sgen(x0V,x1V),sgen(x0V,x2V),sgen(x3V,x2V)),
		Rule(62, Value(0.500000, Token(62)), sgen(x3V,x1V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x0V,x2V)),
		Rule(63, Value(0.500000, Token(63)), sgen(x3V,x1V),parent(x0V,x2V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(64, Value(0.500000, Token(64)), sgen(x3V,x1V),parent(x0V,x2V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(65, Value(0.500000, Token(65)), sgen(x3V,x1V),parent(x0V,x1V),sgen(x0V,x2V),sgen(x3V,x2V)),
		Rule(66, Value(0.500000, Token(66)), sgen(x3V,x1V),parent(x0V,x1V),parent(x3V,x2V),sgen(x0V,x2V)),
		Rule(67, Value(0.500000, Token(67)), sgen(x3V,x1V),parent(x0V,x1V),parent(x0V,x2V),sgen(x3V,x2V)),
		Rule(68, Value(0.500000, Token(68)), sgen(x3V,x1V),parent(x0V,x1V),parent(x0V,x2V),parent(x3V,x2V)),
		Rule(69, Value(0.500000, Token(69)), sgen(x0V,x2V),sgen(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(70, Value(0.500000, Token(70)), sgen(x0V,x2V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x2V,x0V)),
		Rule(71, Value(0.500000, Token(71)), sgen(x0V,x2V),parent(x2V,x0V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(72, Value(0.500000, Token(72)), sgen(x0V,x2V),parent(x2V,x0V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(73, Value(0.500000, Token(73)), sgen(x0V,x2V),parent(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(74, Value(0.500000, Token(74)), sgen(x0V,x2V),parent(x0V,x1V),parent(x3V,x2V),sgen(x2V,x0V)),
		Rule(75, Value(0.500000, Token(75)), sgen(x0V,x2V),parent(x0V,x1V),parent(x2V,x0V),sgen(x3V,x2V)),
		Rule(76, Value(0.500000, Token(76)), sgen(x0V,x2V),parent(x0V,x1V),parent(x2V,x0V),parent(x3V,x2V)),
		Rule(77, Value(0.500000, Token(77)), sgen(x0V,x3V),sgen(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(78, Value(0.500000, Token(78)), sgen(x0V,x3V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x2V,x0V)),
		Rule(79, Value(0.500000, Token(79)), sgen(x0V,x3V),parent(x2V,x0V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(80, Value(0.500000, Token(80)), sgen(x0V,x3V),parent(x2V,x0V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(81, Value(0.500000, Token(81)), sgen(x0V,x3V),parent(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(82, Value(0.500000, Token(82)), sgen(x0V,x3V),parent(x0V,x1V),parent(x3V,x2V),sgen(x2V,x0V)),
		Rule(83, Value(0.500000, Token(83)), sgen(x0V,x3V),parent(x0V,x1V),parent(x2V,x0V),sgen(x3V,x2V)),
		Rule(84, Value(0.500000, Token(84)), sgen(x0V,x3V),parent(x0V,x1V),parent(x2V,x0V),parent(x3V,x2V)),
		Rule(85, Value(0.500000, Token(85)), sgen(x1V,x2V),sgen(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(86, Value(0.500000, Token(86)), sgen(x1V,x2V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x2V,x0V)),
		Rule(87, Value(0.500000, Token(87)), sgen(x1V,x2V),parent(x2V,x0V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(88, Value(0.500000, Token(88)), sgen(x1V,x2V),parent(x2V,x0V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(89, Value(0.500000, Token(89)), sgen(x1V,x2V),parent(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(90, Value(0.500000, Token(90)), sgen(x1V,x2V),parent(x0V,x1V),parent(x3V,x2V),sgen(x2V,x0V)),
		Rule(91, Value(0.500000, Token(91)), sgen(x1V,x2V),parent(x0V,x1V),parent(x2V,x0V),sgen(x3V,x2V)),
		Rule(92, Value(0.500000, Token(92)), sgen(x1V,x2V),parent(x0V,x1V),parent(x2V,x0V),parent(x3V,x2V)),
		Rule(93, Value(0.500000, Token(93)), sgen(x1V,x3V),sgen(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(94, Value(0.500000, Token(94)), sgen(x1V,x3V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x2V,x0V)),
		Rule(95, Value(0.500000, Token(95)), sgen(x1V,x3V),parent(x2V,x0V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(96, Value(0.500000, Token(96)), sgen(x1V,x3V),parent(x2V,x0V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(97, Value(0.500000, Token(97)), sgen(x1V,x3V),parent(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(98, Value(0.500000, Token(98)), sgen(x1V,x3V),parent(x0V,x1V),parent(x3V,x2V),sgen(x2V,x0V)),
		Rule(99, Value(0.500000, Token(99)), sgen(x1V,x3V),parent(x0V,x1V),parent(x2V,x0V),sgen(x3V,x2V)),
		Rule(100, Value(0.500000, Token(100)), sgen(x1V,x3V),parent(x0V,x1V),parent(x2V,x0V),parent(x3V,x2V)),
		Rule(101, Value(0.500000, Token(101)), sgen(x2V,x0V),parent(x2V,x0V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(102, Value(0.500000, Token(102)), sgen(x2V,x0V),parent(x2V,x0V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(103, Value(0.500000, Token(103)), sgen(x2V,x0V),parent(x0V,x1V),parent(x2V,x0V),sgen(x3V,x2V)),
		Rule(104, Value(0.500000, Token(104)), sgen(x2V,x0V),parent(x0V,x1V),parent(x2V,x0V),parent(x3V,x2V)),
		Rule(105, Value(0.500000, Token(105)), sgen(x2V,x1V),sgen(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(106, Value(0.500000, Token(106)), sgen(x2V,x1V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x2V,x0V)),
		Rule(107, Value(0.500000, Token(107)), sgen(x2V,x1V),parent(x2V,x0V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(108, Value(0.500000, Token(108)), sgen(x2V,x1V),parent(x2V,x0V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(109, Value(0.500000, Token(109)), sgen(x2V,x1V),parent(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(110, Value(0.500000, Token(110)), sgen(x2V,x1V),parent(x0V,x1V),parent(x3V,x2V),sgen(x2V,x0V)),
		Rule(111, Value(0.500000, Token(111)), sgen(x2V,x1V),parent(x0V,x1V),parent(x2V,x0V),sgen(x3V,x2V)),
		Rule(112, Value(0.500000, Token(112)), sgen(x2V,x1V),parent(x0V,x1V),parent(x2V,x0V),parent(x3V,x2V)),
		Rule(113, Value(0.500000, Token(113)), sgen(x3V,x0V),sgen(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(114, Value(0.500000, Token(114)), sgen(x3V,x0V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x2V,x0V)),
		Rule(115, Value(0.500000, Token(115)), sgen(x3V,x0V),parent(x2V,x0V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(116, Value(0.500000, Token(116)), sgen(x3V,x0V),parent(x2V,x0V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(117, Value(0.500000, Token(117)), sgen(x3V,x0V),parent(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(118, Value(0.500000, Token(118)), sgen(x3V,x0V),parent(x0V,x1V),parent(x3V,x2V),sgen(x2V,x0V)),
		Rule(119, Value(0.500000, Token(119)), sgen(x3V,x0V),parent(x0V,x1V),parent(x2V,x0V),sgen(x3V,x2V)),
		Rule(120, Value(0.500000, Token(120)), sgen(x3V,x0V),parent(x0V,x1V),parent(x2V,x0V),parent(x3V,x2V)),
		Rule(121, Value(0.500000, Token(121)), sgen(x3V,x1V),sgen(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(122, Value(0.500000, Token(122)), sgen(x3V,x1V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x2V,x0V)),
		Rule(123, Value(0.500000, Token(123)), sgen(x3V,x1V),parent(x2V,x0V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(124, Value(0.500000, Token(124)), sgen(x3V,x1V),parent(x2V,x0V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(125, Value(0.500000, Token(125)), sgen(x3V,x1V),parent(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
		Rule(126, Value(0.500000, Token(126)), sgen(x3V,x1V),parent(x0V,x1V),parent(x3V,x2V),sgen(x2V,x0V)),
		Rule(127, Value(0.500000, Token(127)), sgen(x3V,x1V),parent(x0V,x1V),parent(x2V,x0V),sgen(x3V,x2V)),
		Rule(128, Value(0.500000, Token(128)), sgen(x3V,x1V),parent(x0V,x1V),parent(x2V,x0V),parent(x3V,x2V)),
		Rule(129, Value(0.500000, Token(129)), sgen(x0V,x3V),sgen(x0V,x1V),sgen(x1V,x2V),sgen(x3V,x2V)),
		Rule(130, Value(0.500000, Token(130)), sgen(x0V,x3V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x1V,x2V)),
		Rule(131, Value(0.500000, Token(131)), sgen(x0V,x3V),parent(x1V,x2V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(132, Value(0.500000, Token(132)), sgen(x0V,x3V),parent(x1V,x2V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(133, Value(0.500000, Token(133)), sgen(x0V,x3V),parent(x0V,x1V),sgen(x1V,x2V),sgen(x3V,x2V)),
		Rule(134, Value(0.500000, Token(134)), sgen(x0V,x3V),parent(x0V,x1V),parent(x3V,x2V),sgen(x1V,x2V)),
		Rule(135, Value(0.500000, Token(135)), sgen(x0V,x3V),parent(x0V,x1V),parent(x1V,x2V),sgen(x3V,x2V)),
		Rule(136, Value(0.500000, Token(136)), sgen(x0V,x3V),parent(x0V,x1V),parent(x1V,x2V),parent(x3V,x2V)),
		Rule(137, Value(0.500000, Token(137)), sgen(x1V,x2V),parent(x1V,x2V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(138, Value(0.500000, Token(138)), sgen(x1V,x2V),parent(x0V,x1V),parent(x1V,x2V),sgen(x3V,x2V)),
		Rule(139, Value(0.500000, Token(139)), sgen(x3V,x0V),sgen(x0V,x1V),sgen(x1V,x2V),sgen(x3V,x2V)),
		Rule(140, Value(0.500000, Token(140)), sgen(x3V,x0V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x1V,x2V)),
		Rule(141, Value(0.500000, Token(141)), sgen(x3V,x0V),parent(x1V,x2V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(142, Value(0.500000, Token(142)), sgen(x3V,x0V),parent(x1V,x2V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(143, Value(0.500000, Token(143)), sgen(x3V,x0V),parent(x0V,x1V),sgen(x1V,x2V),sgen(x3V,x2V)),
		Rule(144, Value(0.500000, Token(144)), sgen(x3V,x0V),parent(x0V,x1V),parent(x3V,x2V),sgen(x1V,x2V)),
		Rule(145, Value(0.500000, Token(145)), sgen(x3V,x0V),parent(x0V,x1V),parent(x1V,x2V),sgen(x3V,x2V)),
		Rule(146, Value(0.500000, Token(146)), sgen(x3V,x0V),parent(x0V,x1V),parent(x1V,x2V),parent(x3V,x2V)),
		Rule(147, Value(0.500000, Token(147)), sgen(x3V,x1V),sgen(x0V,x1V),sgen(x1V,x2V),sgen(x3V,x2V)),
		Rule(148, Value(0.500000, Token(148)), sgen(x3V,x1V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x1V,x2V)),
		Rule(149, Value(0.500000, Token(149)), sgen(x3V,x1V),parent(x1V,x2V),sgen(x0V,x1V),sgen(x3V,x2V)),
		Rule(150, Value(0.500000, Token(150)), sgen(x3V,x1V),parent(x1V,x2V),parent(x3V,x2V),sgen(x0V,x1V)),
		Rule(151, Value(0.500000, Token(151)), sgen(x3V,x1V),parent(x0V,x1V),sgen(x1V,x2V),sgen(x3V,x2V)),
		Rule(152, Value(0.500000, Token(152)), sgen(x3V,x1V),parent(x0V,x1V),parent(x3V,x2V),sgen(x1V,x2V)),
		Rule(153, Value(0.500000, Token(153)), sgen(x3V,x1V),parent(x0V,x1V),parent(x1V,x2V),sgen(x3V,x2V)),
		Rule(154, Value(0.500000, Token(154)), sgen(x3V,x1V),parent(x0V,x1V),parent(x1V,x2V),parent(x3V,x2V)),
    )
// result/nlp/samegen_253.nlp
  val soup1: Set[Rule] = Set(
    Rule(1, Value(0.405, Token(1)), sgen(x1V, x0V),parent(x1V, x0V)),
    Rule(2, Value(0.286, Token(2)), sgen(x1V, x0V),parent(x1V, x0V)),
    Rule(3, Value(0.278, Token(3)), sgen(x1V, x0V),sgen(x1V, x0V)),
    Rule(4, Value(0.131, Token(4)), sgen(x1V, x0V),sgen(x1V, x0V)),
    Rule(5, Value(0.115, Token(5)), sgen(x2V, x0V),parent(x1V, x0V), parent(x2V, x1V)),
    Rule(6, Value(0.079, Token(6)), sgen(x2V, x0V),sgen(x1V, x0V), parent(x2V, x1V)),
    Rule(7, Value(0.053, Token(7)), sgen(x2V, x0V),parent(x1V, x0V), sgen(x2V, x1V)),
    Rule(8, Value(0.036, Token(8)), sgen(x2V, x0V),sgen(x1V, x0V), sgen(x2V, x1V)),
  )
// result/nlp/samegen_34.nlp
  val soup2: Set[Rule] = Set(
    Rule(1, Value(0.691, Token(1)), sgen(x1V, x0V),parent(x1V, x0V)),
    Rule(2, Value(0.501, Token(2)), sgen(x1V, x0V),parent(x1V, x0V)),
    Rule(3, Value(0.344, Token(3)), sgen(x2V, x0V),parent(x1V, x0V), parent(x2V, x1V)),
    Rule(4, Value(0.139, Token(4)), sgen(x1V, x0V),sgen(x1V, x0V)),
    Rule(5, Value(0.069, Token(5)), sgen(x2V, x0V),sgen(x1V, x0V), parent(x2V, x1V)),
  )
// result/nlp/samegen_3519.nlp
  val soup3: Set[Rule] = Set(
    Rule(1, Value(0.487, Token(1)), sgen(x1V, x0V),parent(x1V, x0V)),
    Rule(2, Value(0.357, Token(2)), sgen(x1V, x0V),parent(x1V, x0V)),
    Rule(3, Value(0.256, Token(3)), sgen(x1V, x0V),sgen(x1V, x0V)),
    Rule(4, Value(0.174, Token(4)), sgen(x2V, x0V),parent(x1V, x0V), parent(x2V, x1V)),
    Rule(5, Value(0.092, Token(5)), sgen(x2V, x0V),sgen(x1V, x0V), parent(x2V, x1V)),
    Rule(6, Value(0.091, Token(6)), sgen(x1V, x0V),sgen(x1V, x0V)),
    Rule(7, Value(0.045, Token(7)), sgen(x2V, x0V),parent(x1V, x0V), sgen(x2V, x1V)),
  )
// result/nlp/samegen_42.nlp
  val soup4: Set[Rule] = Set(
    Rule(1, Value(0.611, Token(1)), sgen(x1V, x0V),parent(x1V, x0V)),
    Rule(2, Value(0.445, Token(2)), sgen(x1V, x0V),parent(x1V, x0V)),
    Rule(3, Value(0.271, Token(3)), sgen(x2V, x0V),parent(x1V, x0V), parent(x2V, x1V)),
    Rule(4, Value(0.174, Token(4)), sgen(x1V, x0V),sgen(x1V, x0V)),
    Rule(5, Value(0.077, Token(5)), sgen(x2V, x0V),sgen(x1V, x0V), parent(x2V, x1V)),
    Rule(6, Value(0.037, Token(6)), sgen(x1V, x0V),sgen(x1V, x0V)),
  )
// result/nlp/samegen_481.nlp
  val soup5: Set[Rule] = Set(
    Rule(1, Value(0.503, Token(1)), sgen(x1V, x0V),parent(x1V, x0V)),
    Rule(2, Value(0.388, Token(2)), sgen(x1V, x0V),parent(x1V, x0V)),
    Rule(3, Value(0.259, Token(3)), sgen(x1V, x0V),sgen(x1V, x0V)),
    Rule(4, Value(0.196, Token(4)), sgen(x2V, x0V),parent(x1V, x0V), parent(x2V, x1V)),
    Rule(5, Value(0.101, Token(5)), sgen(x2V, x0V),sgen(x1V, x0V), parent(x2V, x1V)),
    Rule(6, Value(0.082, Token(6)), sgen(x1V, x0V),sgen(x1V, x0V)),
    Rule(7, Value(0.041, Token(7)), sgen(x2V, x0V),parent(x1V, x0V), sgen(x2V, x1V)),
  )
// result/nlp/samegen_499.nlp
  val soup6: Set[Rule] = Set(
    Rule(1, Value(0.587, Token(1)), sgen(x1V, x0V),parent(x1V, x0V)),
    Rule(2, Value(0.437, Token(2)), sgen(x1V, x0V),parent(x1V, x0V)),
    Rule(3, Value(0.258, Token(3)), sgen(x2V, x0V),parent(x1V, x0V), parent(x2V, x1V)),
    Rule(4, Value(0.216, Token(4)), sgen(x1V, x0V),sgen(x1V, x0V)),
    Rule(5, Value(0.095, Token(5)), sgen(x2V, x0V),sgen(x1V, x0V), parent(x2V, x1V)),
    Rule(6, Value(0.055, Token(6)), sgen(x1V, x0V),sgen(x1V, x0V)),
  )
// result/nlp/samegen_591.nlp
  val soup7: Set[Rule] = Set(
    Rule(1, Value(0.568, Token(1)), sgen(x1V, x0V),parent(x1V, x0V)),
    Rule(2, Value(0.439, Token(2)), sgen(x1V, x0V),parent(x1V, x0V)),
    Rule(3, Value(0.290, Token(3)), sgen(x1V, x0V),sgen(x1V, x0V)),
    Rule(4, Value(0.246, Token(4)), sgen(x2V, x0V),parent(x1V, x0V), parent(x2V, x1V)),
    Rule(5, Value(0.125, Token(5)), sgen(x2V, x0V),sgen(x1V, x0V), parent(x2V, x1V)),
    Rule(6, Value(0.106, Token(6)), sgen(x1V, x0V),sgen(x1V, x0V)),
    Rule(7, Value(0.059, Token(7)), sgen(x2V, x0V),parent(x1V, x0V), sgen(x2V, x1V)),
  )
// result/nlp/samegen_6012.nlp
  val soup8: Set[Rule] = Set(
    Rule(1, Value(0.497, Token(1)), sgen(x1V, x0V),parent(x1V, x0V)),
    Rule(2, Value(0.350, Token(2)), sgen(x1V, x0V),parent(x1V, x0V)),
    Rule(3, Value(0.178, Token(3)), sgen(x1V, x0V),sgen(x1V, x0V)),
    Rule(4, Value(0.173, Token(4)), sgen(x2V, x0V),parent(x1V, x0V), parent(x2V, x1V)),
    Rule(5, Value(0.062, Token(5)), sgen(x2V, x0V),sgen(x1V, x0V), parent(x2V, x1V)),
    Rule(6, Value(0.044, Token(6)), sgen(x1V, x0V),sgen(x1V, x0V)),
  )
// result/nlp/samegen_68.nlp
  val soup9: Set[Rule] = Set(
    Rule(1, Value(0.707, Token(1)), sgen(x1V, x0V),parent(x1V, x0V)),
    Rule(2, Value(0.524, Token(2)), sgen(x1V, x0V),parent(x1V, x0V)),
    Rule(3, Value(0.363, Token(3)), sgen(x2V, x0V),parent(x1V, x0V), parent(x2V, x1V)),
    Rule(4, Value(0.144, Token(4)), sgen(x1V, x0V),sgen(x1V, x0V)),
    Rule(5, Value(0.074, Token(5)), sgen(x2V, x0V),sgen(x1V, x0V), parent(x2V, x1V)),
  )
// result/nlp/samegen_991.nlp
  val soup10: Set[Rule] = Set(
    Rule(1, Value(0.517, Token(1)), sgen(x1V, x0V),parent(x1V, x0V)),
    Rule(2, Value(0.397, Token(2)), sgen(x1V, x0V),parent(x1V, x0V)),
    Rule(3, Value(0.223, Token(3)), sgen(x1V, x0V),sgen(x1V, x0V)),
    Rule(4, Value(0.205, Token(4)), sgen(x2V, x0V),parent(x1V, x0V), parent(x2V, x1V)),
    Rule(5, Value(0.088, Token(5)), sgen(x2V, x0V),sgen(x1V, x0V), parent(x2V, x1V)),
    Rule(6, Value(0.076, Token(6)), sgen(x1V, x0V),sgen(x1V, x0V)),
    Rule(7, Value(0.039, Token(7)), sgen(x2V, x0V),parent(x1V, x0V), sgen(x2V, x1V)),
  )

  val soupProg: Program = Program("EscapeSoup", soup)
  val soupProg1: Program = Program("EscapeSoup", soup1)
  val soupProg2: Program = Program("EscapeSoup", soup2)
  val soupProg3: Program = Program("EscapeSoup", soup3)
  val soupProg4: Program = Program("EscapeSoup", soup4)
  val soupProg5: Program = Program("EscapeSoup", soup5)
  val soupProg6: Program = Program("EscapeSoup", soup6)
  val soupProg7: Program = Program("EscapeSoup", soup7)
  val soupProg8: Program = Program("EscapeSoup", soup8)
  val soupProg9: Program = Program("EscapeSoup", soup9)
  val soupProg10: Program = Program("EscapeSoup", soup10)


  test(s"NLP result ${soupProg.name}") {
    val scorer = new Scorer(edb, refOut)
    println("soup1")
    val evaluator1 = SeminaiveEvaluator(soupProg1)
    val out1 = evaluator1(edb)
    val rmse1 = scorer.rmse(out1)
    println(s"RMS Error : $rmse1.")

    println("soup2")
    val evaluator2 = SeminaiveEvaluator(soupProg2)
    val out2 = evaluator2(edb)
    val rmse2 = scorer.rmse(out2)
    println(s"RMS Error : $rmse2.")

    println("soup3")
    val evaluator3 = SeminaiveEvaluator(soupProg3)
    val out3 = evaluator3(edb)
    val rmse3 = scorer.rmse(out3)
    println(s"RMS Error : $rmse3.")

    println("soup4")
    val evaluator4 = SeminaiveEvaluator(soupProg4)
    val out4 = evaluator4(edb)
    val rmse4 = scorer.rmse(out4)
    println(s"RMS Error : $rmse4.")

    println("soup5")
    val evaluator5 = SeminaiveEvaluator(soupProg5)
    val out5 = evaluator5(edb)
    val rmse5 = scorer.rmse(out5)
    println(s"RMS Error : $rmse5.")

    println("soup6")
    val evaluator6 = SeminaiveEvaluator(soupProg6)
    val out6 = evaluator6(edb)
    val rmse6 = scorer.rmse(out6)
    println(s"RMS Error : $rmse6.")

    println("soup7")
    val evaluator7 = SeminaiveEvaluator(soupProg7)
    val out7 = evaluator7(edb)
    val rmse7 = scorer.rmse(out7)
    println(s"RMS Error : $rmse7.")

    println("soup8")
    val evaluator8 = SeminaiveEvaluator(soupProg8)
    val out8 = evaluator8(edb)
    val rmse8 = scorer.rmse(out8)
    println(s"RMS Error : $rmse8.")

    println("soup9")
    val evaluator9 = SeminaiveEvaluator(soupProg9)
    val out9 = evaluator9(edb)
    val rmse9 = scorer.rmse(out9)
    println(s"RMS Error : $rmse9.")

    println("soup10")
    val evaluator10 = SeminaiveEvaluator(soupProg10)
    val out10 = evaluator10(edb)
    val rmse10 = scorer.rmse(out10)
    println(s"RMS Error : $rmse10.")
  }

  val expected = Set(28, 133)
  val maxVarCount: Int = 20
}
