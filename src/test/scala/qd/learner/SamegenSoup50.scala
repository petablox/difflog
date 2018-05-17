package qd
package learner
import org.scalatest.{FunSuite, Ignore}

class SamegenSoup50 extends Problem {
  override val name = "samegen"
  val VSet = Range(0, 10).map(i => Atom(i)).toSet
  val V = Domain("V", VSet)
  val parent = Relation("parent", V,V)
  val sgen = Relation("sgen", V,V)
  val parentTuples = Set((2, 1),(3, 1),(4, 2),(5, 2),(6, 3),(7, 3),(8, 9)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
  val sgenTuples = Set((2, 2),(3, 3),(4, 4),(5, 5),(6, 6),(7, 7),(2, 3),(3, 2),(4, 5),(4, 6),(4, 7),(5, 4),(5, 6),(5, 7),(6, 7),(6, 4),(6, 5),(6, 7),(7, 4),(7, 5),(7, 6),(8, 8)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
  override val edb = Config(
    parent -> (Instance(parent) ++ parentTuples.map(t => t -> One).toMap),
    )
  override val refOut = Config(
    sgen -> (Instance(sgen) ++ sgenTuples.map(t => t -> One).toMap),
    )
  val x1V = Variable("x1V",V)
  val x3V = Variable("x3V",V)
  val x0V = Variable("x0V",V)
  val x2V = Variable("x2V",V)
  val soup = Set(
    Rule(0, Value(0.5, Token(0)), sgen(x0V,x1V),parent(x0V,x1V),sgen(x0V,x2V)),
    Rule(1, Value(0.5, Token(1)), sgen(x1V,x2V),sgen(x0V,x1V),sgen(x0V,x2V)),
    Rule(2, Value(0.5, Token(2)), sgen(x1V,x2V),parent(x0V,x2V),sgen(x0V,x1V)),
    Rule(3, Value(0.5, Token(3)), sgen(x1V,x2V),parent(x0V,x1V),sgen(x0V,x2V)),
    Rule(4, Value(0.5, Token(4)), sgen(x1V,x2V),parent(x0V,x1V),parent(x0V,x2V)),
    Rule(5, Value(0.5, Token(5)), sgen(x0V,x1V),parent(x0V,x1V),sgen(x2V,x0V)),
    Rule(6, Value(0.5, Token(6)), sgen(x0V,x1V),parent(x0V,x1V),parent(x2V,x0V)),
    Rule(7, Value(0.5, Token(7)), sgen(x0V,x2V),sgen(x0V,x1V),sgen(x2V,x0V)),
    Rule(11, Value(0.5, Token(11)), sgen(x1V,x0V),sgen(x0V,x1V),sgen(x2V,x0V)),
    Rule(24, Value(0.5, Token(24)), sgen(x2V,x1V),parent(x0V,x1V),parent(x2V,x0V)),
    Rule(25, Value(0.5, Token(25)), sgen(x0V,x2V),sgen(x0V,x1V),sgen(x2V,x1V)),
    Rule(26, Value(0.5, Token(26)), sgen(x0V,x2V),parent(x2V,x1V),sgen(x0V,x1V)),
    Rule(27, Value(0.5, Token(27)), sgen(x0V,x2V),parent(x0V,x1V),sgen(x2V,x1V)),
    Rule(28, Value(0.5, Token(28)), sgen(x0V,x2V),parent(x0V,x1V),parent(x2V,x1V)),
    Rule(29, Value(0.5, Token(29)), sgen(x0V,x2V),parent(x0V,x2V),sgen(x0V,x1V),sgen(x2V,x3V)),
    Rule(30, Value(0.5, Token(30)), sgen(x0V,x2V),parent(x0V,x2V),parent(x2V,x3V),sgen(x0V,x1V)),
    Rule(31, Value(0.5, Token(31)), sgen(x0V,x3V),parent(x0V,x2V),sgen(x0V,x1V),sgen(x2V,x3V)),
    Rule(32, Value(0.5, Token(32)), sgen(x0V,x3V),parent(x0V,x2V),parent(x2V,x3V),sgen(x0V,x1V)),
    Rule(33, Value(0.5, Token(33)), sgen(x0V,x3V),parent(x0V,x1V),sgen(x0V,x2V),sgen(x2V,x3V)),
    Rule(34, Value(0.5, Token(34)), sgen(x0V,x3V),parent(x0V,x1V),parent(x2V,x3V),sgen(x0V,x2V)),
    Rule(35, Value(0.5, Token(35)), sgen(x1V,x3V),sgen(x0V,x1V),sgen(x0V,x2V),sgen(x2V,x3V)),
    Rule(36, Value(0.5, Token(36)), sgen(x1V,x3V),parent(x2V,x3V),sgen(x0V,x1V),sgen(x0V,x2V)),
    Rule(37, Value(0.5, Token(37)), sgen(x1V,x3V),parent(x0V,x2V),sgen(x0V,x1V),sgen(x2V,x3V)),
    Rule(38, Value(0.5, Token(38)), sgen(x1V,x3V),parent(x0V,x2V),parent(x2V,x3V),sgen(x0V,x1V)),
    Rule(39, Value(0.5, Token(39)), sgen(x1V,x3V),parent(x0V,x1V),sgen(x0V,x2V),sgen(x2V,x3V)),
    Rule(40, Value(0.5, Token(40)), sgen(x1V,x3V),parent(x0V,x1V),parent(x2V,x3V),sgen(x0V,x2V)),
    Rule(41, Value(0.5, Token(41)), sgen(x1V,x3V),parent(x0V,x1V),parent(x0V,x2V),sgen(x2V,x3V)),
    Rule(42, Value(0.5, Token(42)), sgen(x1V,x3V),parent(x0V,x1V),parent(x0V,x2V),parent(x2V,x3V)),
    Rule(43, Value(0.5, Token(43)), sgen(x2V,x1V),sgen(x0V,x1V),sgen(x0V,x2V),sgen(x2V,x3V)),
    Rule(47, Value(0.5, Token(47)), sgen(x2V,x1V),parent(x0V,x1V),sgen(x0V,x2V),sgen(x2V,x3V)),
    Rule(48, Value(0.5, Token(48)), sgen(x2V,x1V),parent(x0V,x1V),parent(x2V,x3V),sgen(x0V,x2V)),
    Rule(49, Value(0.5, Token(49)), sgen(x2V,x1V),parent(x0V,x1V),parent(x0V,x2V),sgen(x2V,x3V)),
    Rule(50, Value(0.5, Token(50)), sgen(x2V,x1V),parent(x0V,x1V),parent(x0V,x2V),parent(x2V,x3V)),
    Rule(51, Value(0.5, Token(51)), sgen(x3V,x1V),sgen(x0V,x1V),sgen(x0V,x2V),sgen(x2V,x3V)),
    Rule(52, Value(0.5, Token(52)), sgen(x3V,x1V),parent(x2V,x3V),sgen(x0V,x1V),sgen(x0V,x2V)),
    Rule(53, Value(0.5, Token(53)), sgen(x3V,x1V),parent(x0V,x2V),sgen(x0V,x1V),sgen(x2V,x3V)),
    Rule(54, Value(0.5, Token(54)), sgen(x3V,x1V),parent(x0V,x2V),parent(x2V,x3V),sgen(x0V,x1V)),
    Rule(55, Value(0.5, Token(55)), sgen(x3V,x1V),parent(x0V,x1V),sgen(x0V,x2V),sgen(x2V,x3V)),
    Rule(56, Value(0.5, Token(56)), sgen(x3V,x1V),parent(x0V,x1V),parent(x2V,x3V),sgen(x0V,x2V)),
    Rule(57, Value(0.5, Token(57)), sgen(x3V,x1V),parent(x0V,x1V),parent(x0V,x2V),sgen(x2V,x3V)),
    Rule(58, Value(0.5, Token(58)), sgen(x3V,x1V),parent(x0V,x1V),parent(x0V,x2V),parent(x2V,x3V)),
    Rule(59, Value(0.5, Token(59)), sgen(x0V,x2V),parent(x0V,x2V),sgen(x0V,x1V),sgen(x3V,x2V)),
    Rule(60, Value(0.5, Token(60)), sgen(x3V,x1V),sgen(x0V,x1V),sgen(x0V,x2V),sgen(x3V,x2V)),
    Rule(61, Value(0.5, Token(61)), sgen(x3V,x1V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x0V,x2V)),
    Rule(62, Value(0.5, Token(62)), sgen(x3V,x1V),parent(x0V,x2V),sgen(x0V,x1V),sgen(x3V,x2V)),
    Rule(63, Value(0.5, Token(63)), sgen(x3V,x1V),parent(x0V,x2V),parent(x3V,x2V),sgen(x0V,x1V)),
    Rule(106, Value(0.5, Token(106)), sgen(x2V,x1V),parent(x2V,x0V),sgen(x0V,x1V),sgen(x3V,x2V)),
    Rule(107, Value(0.5, Token(107)), sgen(x2V,x1V),parent(x2V,x0V),parent(x3V,x2V),sgen(x0V,x1V)),
    Rule(108, Value(0.5, Token(108)), sgen(x2V,x1V),parent(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
    Rule(109, Value(0.5, Token(109)), sgen(x2V,x1V),parent(x0V,x1V),parent(x3V,x2V),sgen(x2V,x0V)),
    Rule(110, Value(0.5, Token(110)), sgen(x2V,x1V),parent(x0V,x1V),parent(x2V,x0V),sgen(x3V,x2V)),
    Rule(111, Value(0.5, Token(111)), sgen(x2V,x1V),parent(x0V,x1V),parent(x2V,x0V),parent(x3V,x2V)),
    Rule(112, Value(0.5, Token(112)), sgen(x3V,x0V),sgen(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
    Rule(113, Value(0.5, Token(113)), sgen(x3V,x0V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x2V,x0V)),
    Rule(114, Value(0.5, Token(114)), sgen(x3V,x0V),parent(x2V,x0V),sgen(x0V,x1V),sgen(x3V,x2V)),
    Rule(115, Value(0.5, Token(115)), sgen(x3V,x0V),parent(x2V,x0V),parent(x3V,x2V),sgen(x0V,x1V)),
    Rule(116, Value(0.5, Token(116)), sgen(x3V,x0V),parent(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
    Rule(117, Value(0.5, Token(117)), sgen(x3V,x0V),parent(x0V,x1V),parent(x3V,x2V),sgen(x2V,x0V)),
    Rule(118, Value(0.5, Token(118)), sgen(x3V,x0V),parent(x0V,x1V),parent(x2V,x0V),sgen(x3V,x2V)),
    Rule(119, Value(0.5, Token(119)), sgen(x3V,x0V),parent(x0V,x1V),parent(x2V,x0V),parent(x3V,x2V)),
    Rule(120, Value(0.5, Token(120)), sgen(x3V,x1V),sgen(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
    Rule(121, Value(0.5, Token(121)), sgen(x3V,x1V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x2V,x0V)),
    Rule(122, Value(0.5, Token(122)), sgen(x3V,x1V),parent(x2V,x0V),sgen(x0V,x1V),sgen(x3V,x2V)),
    Rule(124, Value(0.5, Token(124)), sgen(x3V,x1V),parent(x0V,x1V),sgen(x2V,x0V),sgen(x3V,x2V)),
    Rule(125, Value(0.5, Token(125)), sgen(x3V,x1V),parent(x0V,x1V),parent(x3V,x2V),sgen(x2V,x0V)),
    Rule(126, Value(0.5, Token(126)), sgen(x3V,x1V),parent(x0V,x1V),parent(x2V,x0V),sgen(x3V,x2V)),
    Rule(127, Value(0.5, Token(127)), sgen(x3V,x1V),parent(x0V,x1V),parent(x2V,x0V),parent(x3V,x2V)),
    Rule(128, Value(0.5, Token(128)), sgen(x0V,x3V),sgen(x0V,x1V),sgen(x1V,x2V),sgen(x3V,x2V)),
    Rule(129, Value(0.5, Token(129)), sgen(x0V,x3V),parent(x3V,x2V),sgen(x0V,x1V),sgen(x1V,x2V)),
    Rule(130, Value(0.5, Token(130)), sgen(x0V,x3V),parent(x1V,x2V),sgen(x0V,x1V),sgen(x3V,x2V)),
    Rule(131, Value(0.5, Token(131)), sgen(x0V,x3V),parent(x1V,x2V),parent(x3V,x2V),sgen(x0V,x1V)),
    Rule(132, Value(0.5, Token(132)), sgen(x0V,x3V),parent(x0V,x1V),sgen(x1V,x2V),sgen(x3V,x2V)),
    Rule(133, Value(0.5, Token(133)), sgen(x0V,x3V),parent(x0V,x1V),parent(x3V,x2V),sgen(x1V,x2V)),
    Rule(134, Value(0.5, Token(134)), sgen(x0V,x3V),parent(x0V,x1V),parent(x1V,x2V),sgen(x3V,x2V)),
    Rule(136, Value(0.5, Token(136)), sgen(x1V,x2V),parent(x1V,x2V),sgen(x0V,x1V),sgen(x3V,x2V)),
    Rule(148, Value(0.5, Token(148)), sgen(x3V,x1V),parent(x1V,x2V),sgen(x0V,x1V),sgen(x3V,x2V)),
    Rule(152, Value(0.5, Token(152)), sgen(x3V,x1V),parent(x0V,x1V),parent(x1V,x2V),sgen(x3V,x2V)),
    Rule(153, Value(0.5, Token(153)), sgen(x3V,x1V),parent(x0V,x1V),parent(x1V,x2V),parent(x3V,x2V)),
    )
  override val expected = Set(28, 133)
  override val maxVarCount: Int = 20
}
