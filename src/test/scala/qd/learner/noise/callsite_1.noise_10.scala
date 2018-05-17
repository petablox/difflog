package qd
package learner
import org.scalatest.{FunSuite, Ignore}
@Ignore
class Gencallsite_1_10 extends Problem {
	override val name = "callsite_1"
	val CSet = Range(0, 4).map(i => Atom(i)).toSet
	val C = Domain("C", CSet)
	val FSet = Range(0, 3).map(i => Atom(i)).toSet
	val F = Domain("F", FSet)
	val HSet = Range(0, 12).map(i => Atom(i)).toSet
	val H = Domain("H", HSet)
	val MSet = Range(0, 3).map(i => Atom(i)).toSet
	val M = Domain("M", MSet)
	val VSet = Range(0, 12).map(i => Atom(i)).toSet
	val V = Domain("V", VSet)
	val ZSet = Range(0, 3).map(i => Atom(i)).toSet
	val Z = Domain("Z", ZSet)
	val load = Relation("load", V,F,V)
	val points_initial = Relation("points_initial", V,H)
	val actual = Relation("actual", H,Z,V)
	val invocation = Relation("invocation", C,H,C,M)
	val assign = Relation("assign", C,V,C,V)
	val store = Relation("store", V,F,V)
	val formal = Relation("formal", M,Z,V)
	val heappointsto = Relation("heappointsto", H,F,H)
	val pointsto = Relation("pointsto", C,V,H)
	val loadTuples = Set((2, 2, 8),(5, 1, 10),(5, 1, 7)).map { case (x0,x1,x2) => DTuple(Atom(x0),Atom(x1),Atom(x2)) }
	val points_initialTuples = Set((1, 1),(2, 2),(5, 1),(9, 9),(11, 11)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val actualTuples = Set((1, 0, 5),(1, 1, 7),(2, 0, 6),(2, 1, 2)).map { case (x0,x1,x2) => DTuple(Atom(x0),Atom(x1),Atom(x2)) }
	val pointstoTuples = Set((1, 1, 1),(1, 5, 1),(3, 2, 2),(3, 11, 11),(2, 9, 9),(2, 5, 1),(2, 7, 2),(1, 10, 1),(1, 7, 1),(2, 7, 1),(3, 8, 1),(2, 10, 1),(0, 0, 4),(0, 1, 10),(0, 2, 3),(0, 2, 7),(0, 3, 0),(0, 3, 6),(0, 5, 4),(0, 5, 7),(0, 5, 11),(0, 6, 3),(0, 6, 8),(0, 7, 5),(0, 7, 10),(0, 8, 10),(0, 9, 2),(0, 9, 3),(0, 9, 5),(0, 10, 4),(0, 11, 3),(1, 3, 2),(1, 3, 4),(1, 3, 11),(1, 4, 11),(1, 5, 5),(1, 6, 5),(1, 7, 0),(1, 7, 9),(1, 8, 10),(1, 9, 1),(1, 11, 5),(2, 0, 6),(2, 1, 4),(2, 1, 5),(2, 1, 7),(2, 3, 2),(2, 4, 8),(2, 5, 7),(2, 7, 0),(2, 8, 10),(2, 9, 6),(2, 11, 4),(2, 11, 10),(3, 0, 3),(3, 0, 8),(3, 1, 7),(3, 1, 10),(3, 3, 2),(3, 4, 5),(3, 4, 8),(3, 5, 6),(3, 5, 10),(3, 7, 2),(3, 8, 11),(3, 9, 6),(3, 9, 9),(3, 10, 3),(3, 10, 4),(3, 11, 1)).map { case (x0,x1,x2) => DTuple(Atom(x0),Atom(x1),Atom(x2)) }
	val invocationTuples = Set((1, 1, 2, 1),(3, 2, 2, 1),(2, 9, 1, 1),(3, 11, 1, 1)).map { case (x0,x1,x2,x3) => DTuple(Atom(x0),Atom(x1),Atom(x2),Atom(x3)) }
	val heappointstoTuples = Set((1, 1, 1),(2, 2, 1),(1, 2, 1),(9, 2, 1),(0, 0, 8),(0, 0, 9),(0, 1, 0),(0, 1, 3),(0, 1, 8),(0, 1, 9),(0, 2, 0),(0, 2, 2),(0, 2, 5),(0, 2, 9),(1, 0, 9),(1, 1, 0),(2, 0, 0),(2, 0, 8),(2, 1, 2),(2, 2, 4),(3, 0, 11),(3, 1, 4),(3, 2, 0),(3, 2, 3),(3, 2, 6),(3, 2, 10),(4, 0, 7),(4, 0, 8),(4, 1, 1),(4, 1, 4),(4, 2, 0),(4, 2, 3),(5, 0, 4),(5, 2, 4),(5, 2, 6),(5, 2, 8),(6, 0, 3),(6, 2, 2),(7, 0, 7),(7, 0, 11),(7, 1, 1),(7, 2, 3),(7, 2, 11),(8, 1, 4),(8, 1, 7),(9, 1, 1),(9, 1, 11),(9, 2, 11),(10, 0, 3),(10, 0, 5),(10, 0, 11),(10, 2, 11),(11, 0, 2),(11, 0, 4),(11, 0, 10),(11, 1, 8),(11, 2, 11)).map { case (x0,x1,x2) => DTuple(Atom(x0),Atom(x1),Atom(x2)) }
	val assignTuples = Set((2, 5, 1, 5),(2, 7, 1, 7),(2, 5, 3, 6),(2, 7, 3, 2)).map { case (x0,x1,x2,x3) => DTuple(Atom(x0),Atom(x1),Atom(x2),Atom(x3)) }
	val storeTuples = Set((7, 2, 5),(5, 1, 1),(9, 2, 5),(9, 1, 11)).map { case (x0,x1,x2) => DTuple(Atom(x0),Atom(x1),Atom(x2)) }
	val formalTuples = Set((1, 0, 5),(1, 1, 7),(2, 0, 3),(2, 1, 6)).map { case (x0,x1,x2) => DTuple(Atom(x0),Atom(x1),Atom(x2)) }
override val edb = Config(
load -> (Instance(load) ++ loadTuples.map(t => t -> One).toMap),
points_initial -> (Instance(points_initial) ++ points_initialTuples.map(t => t -> One).toMap),
actual -> (Instance(actual) ++ actualTuples.map(t => t -> One).toMap),
invocation -> (Instance(invocation) ++ invocationTuples.map(t => t -> One).toMap),
assign -> (Instance(assign) ++ assignTuples.map(t => t -> One).toMap),
store -> (Instance(store) ++ storeTuples.map(t => t -> One).toMap),
formal -> (Instance(formal) ++ formalTuples.map(t => t -> One).toMap),
)
override val refOut = Config(
heappointsto -> (Instance(heappointsto) ++ heappointstoTuples.map(t => t -> One).toMap),
pointsto -> (Instance(pointsto) ++ pointstoTuples.map(t => t -> One).toMap),
)
	val x3F = Variable("x3F",F)
	val x3C = Variable("x3C",C)
	val x4F = Variable("x4F",F)
	val x6V = Variable("x6V",V)
	val x5C = Variable("x5C",C)
	val x1V = Variable("x1V",V)
	val x4M = Variable("x4M",M)
	val x0V = Variable("x0V",V)
	val x4H = Variable("x4H",H)
	val x3M = Variable("x3M",M)
	val x5H = Variable("x5H",H)
	val x1Z = Variable("x1Z",Z)
	val x2V = Variable("x2V",V)
	val x4C = Variable("x4C",C)
	val x2H = Variable("x2H",H)
	val x0C = Variable("x0C",C)
	val x3V = Variable("x3V",V)
	val x5V = Variable("x5V",V)
	val x4V = Variable("x4V",V)
	val x0H = Variable("x0H",H)
	val x2C = Variable("x2C",C)
	val x2F = Variable("x2F",F)
	val x1H = Variable("x1H",H)
	val x1F = Variable("x1F",F)
	val x3H = Variable("x3H",H)
	val soup = Set(
		Rule(0, Value(0.5, Token(0)), pointsto(x2C,x0V,x1H),invocation(x2C,x1H,x3C,x4M),points_initial(x0V,x1H)),
		Rule(1, Value(0.5, Token(1)), pointsto(x3C,x0V,x1H),invocation(x2C,x1H,x3C,x4M),points_initial(x0V,x1H)),
		Rule(2, Value(0.5, Token(2)), pointsto(x2C,x0V,x1H),assign(x2C,x0V,x3C,x4V),points_initial(x0V,x1H)),
		Rule(3, Value(0.5, Token(3)), pointsto(x2C,x4V,x1H),assign(x2C,x0V,x3C,x4V),points_initial(x0V,x1H)),
		Rule(4, Value(0.5, Token(4)), pointsto(x3C,x0V,x1H),assign(x2C,x0V,x3C,x4V),points_initial(x0V,x1H)),
		Rule(5, Value(0.5, Token(5)), pointsto(x3C,x4V,x1H),assign(x2C,x0V,x3C,x4V),points_initial(x0V,x1H)),
		Rule(6, Value(0.5, Token(6)), pointsto(x2C,x0V,x1H),assign(x2C,x3V,x4C,x0V),points_initial(x0V,x1H)),
		Rule(7, Value(0.5, Token(7)), pointsto(x2C,x3V,x1H),assign(x2C,x3V,x4C,x0V),points_initial(x0V,x1H)),
		Rule(8, Value(0.5, Token(8)), pointsto(x4C,x0V,x1H),assign(x2C,x3V,x4C,x0V),points_initial(x0V,x1H)),
		Rule(9, Value(0.5, Token(9)), pointsto(x4C,x3V,x1H),assign(x2C,x3V,x4C,x0V),points_initial(x0V,x1H)),
		Rule(10, Value(0.5, Token(10)), pointsto(x2C,x0V,x1H),points_initial(x0V,x1H),pointsto(x2C,x0V,x3H)),
		Rule(11, Value(0.5, Token(11)), pointsto(x2C,x0V,x1H),points_initial(x0V,x1H),pointsto(x2C,x3V,x1H)),
		Rule(12, Value(0.5, Token(12)), pointsto(x3C,x2V,x0H),actual(x0H,x1Z,x2V),assign(x3C,x2V,x4C,x5V)),
		Rule(13, Value(0.5, Token(13)), pointsto(x3C,x4V,x0H),actual(x0H,x1Z,x2V),assign(x3C,x4V,x5C,x2V)),
		Rule(14, Value(0.5, Token(14)), pointsto(x3C,x2V,x0H),actual(x0H,x1Z,x2V),pointsto(x3C,x4V,x0H)),
		Rule(15, Value(0.5, Token(15)), pointsto(x3C,x2V,x0H),actual(x0H,x1Z,x2V),pointsto(x3C,x2V,x4H)),
		Rule(16, Value(0.5, Token(16)), pointsto(x3C,x2V,x4H),pointsto(x3C,x0V,x4H),store(x0V,x1F,x2V)),
		Rule(17, Value(0.5, Token(17)), pointsto(x3C,x2V,x4H),load(x0V,x1F,x2V),pointsto(x3C,x0V,x4H)),
		Rule(18, Value(0.5, Token(18)), pointsto(x3C,x0V,x4H),pointsto(x3C,x2V,x4H),store(x0V,x1F,x2V)),
		Rule(19, Value(0.5, Token(19)), pointsto(x3C,x0V,x4H),load(x0V,x1F,x2V),pointsto(x3C,x2V,x4H)),
		Rule(20, Value(0.5, Token(20)), pointsto(x4C,x5V,x1H),assign(x4C,x5V,x0C,x6V),invocation(x0C,x1H,x2C,x3M)),
		Rule(21, Value(0.5, Token(21)), pointsto(x0C,x5V,x1H),invocation(x0C,x1H,x2C,x3M),pointsto(x4C,x5V,x1H)),
		Rule(22, Value(0.5, Token(22)), pointsto(x0C,x4V,x1H),invocation(x0C,x1H,x2C,x3M),pointsto(x2C,x4V,x5H)),
		Rule(23, Value(0.5, Token(23)), pointsto(x0C,x4V,x5H),invocation(x0C,x1H,x2C,x3M),pointsto(x2C,x4V,x5H)),
		Rule(24, Value(0.5, Token(24)), pointsto(x0C,x4V,x5H),assign(x0C,x1V,x2C,x3V),pointsto(x2C,x4V,x5H)),
		Rule(25, Value(0.5, Token(25)), pointsto(x0C,x1V,x5H),assign(x0C,x1V,x2C,x3V),pointsto(x0C,x4V,x5H)),
		Rule(26, Value(0.5, Token(26)), pointsto(x0C,x3V,x5H),assign(x0C,x1V,x2C,x3V),pointsto(x0C,x4V,x5H)),
		Rule(27, Value(0.5, Token(27)), pointsto(x2C,x1V,x5H),assign(x0C,x1V,x2C,x3V),pointsto(x0C,x4V,x5H)),
		Rule(28, Value(0.5, Token(28)), pointsto(x0C,x1V,x5H),assign(x0C,x1V,x2C,x3V),pointsto(x4C,x1V,x5H)),
		Rule(29, Value(0.5, Token(29)), pointsto(x0C,x3V,x5H),assign(x0C,x1V,x2C,x3V),pointsto(x4C,x1V,x5H)),
		Rule(30, Value(0.5, Token(30)), pointsto(x2C,x1V,x5H),assign(x0C,x1V,x2C,x3V),pointsto(x4C,x1V,x5H)),
		Rule(31, Value(0.5, Token(31)), pointsto(x0C,x1V,x5H),assign(x0C,x1V,x2C,x3V),pointsto(x2C,x4V,x5H)),
		Rule(32, Value(0.5, Token(32)), pointsto(x0C,x3V,x5H),assign(x0C,x1V,x2C,x3V),pointsto(x2C,x4V,x5H)),
		Rule(33, Value(0.5, Token(33)), pointsto(x2C,x1V,x5H),assign(x0C,x1V,x2C,x3V),pointsto(x2C,x4V,x5H)),
		Rule(34, Value(0.5, Token(34)), pointsto(x2C,x3V,x5H),assign(x0C,x1V,x2C,x3V),pointsto(x2C,x4V,x5H)),
		Rule(35, Value(0.5, Token(35)), pointsto(x0C,x1V,x5H),assign(x0C,x1V,x2C,x3V),pointsto(x4C,x3V,x5H)),
		Rule(36, Value(0.5, Token(36)), pointsto(x0C,x3V,x5H),assign(x0C,x1V,x2C,x3V),pointsto(x4C,x3V,x5H)),
		Rule(37, Value(0.5, Token(37)), pointsto(x2C,x1V,x5H),assign(x0C,x1V,x2C,x3V),pointsto(x4C,x3V,x5H)),
		Rule(38, Value(0.5, Token(38)), pointsto(x2C,x3V,x5H),assign(x0C,x1V,x2C,x3V),pointsto(x4C,x3V,x5H)),
		Rule(39, Value(0.5, Token(39)), pointsto(x4C,x1V,x5H),assign(x0C,x1V,x2C,x3V),pointsto(x4C,x3V,x5H)),
		Rule(40, Value(0.5, Token(40)), pointsto(x3C,x4V,x2H),heappointsto(x0H,x1F,x2H),pointsto(x3C,x4V,x0H)),
		Rule(41, Value(0.5, Token(41)), pointsto(x3C,x4V,x0H),heappointsto(x0H,x1F,x2H),pointsto(x3C,x4V,x2H)),
		Rule(42, Value(0.5, Token(42)), pointsto(x0C,x1V,x4H),pointsto(x0C,x1V,x2H),pointsto(x0C,x3V,x4H)),
		Rule(43, Value(0.5, Token(43)), pointsto(x0C,x1V,x4H),pointsto(x0C,x1V,x2H),pointsto(x3C,x1V,x4H)),
		Rule(44, Value(0.5, Token(44)), pointsto(x0C,x4V,x2H),pointsto(x0C,x1V,x2H),pointsto(x3C,x4V,x2H)),
		Rule(45, Value(0.5, Token(45)), pointsto(x2C,x5V,x1H),assign(x2C,x5V,x0C,x6V),invocation(x0C,x1H,x2C,x3M)),
		Rule(46, Value(0.5, Token(46)), pointsto(x4C,x5V,x1H),assign(x4C,x5V,x0C,x6V),invocation(x0C,x1H,x0C,x3M)),
		Rule(47, Value(0.5, Token(47)), pointsto(x2C,x4V,x5H),assign(x0C,x4V,x2C,x3V),pointsto(x0C,x4V,x5H)),
		Rule(48, Value(0.5, Token(48)), pointsto(x2C,x4V,x5H),assign(x0C,x1V,x2C,x4V),pointsto(x0C,x4V,x5H)),
		Rule(49, Value(0.5, Token(49)), pointsto(x0C,x5V,x1H),invocation(x0C,x1H,x4C,x3M),pointsto(x4C,x5V,x1H)),
		Rule(50, Value(0.5, Token(50)), pointsto(x0C,x4V,x5H),assign(x0C,x4V,x2C,x3V),pointsto(x2C,x4V,x5H)),
		Rule(51, Value(0.5, Token(51)), pointsto(x0C,x4V,x5H),assign(x0C,x1V,x2C,x4V),pointsto(x2C,x4V,x5H)),
		Rule(52, Value(0.5, Token(52)), pointsto(x0C,x1V,x5H),assign(x0C,x1V,x2C,x4V),pointsto(x0C,x4V,x5H)),
		Rule(53, Value(0.5, Token(53)), pointsto(x0C,x3V,x5H),assign(x0C,x4V,x2C,x3V),pointsto(x0C,x4V,x5H)),
		Rule(54, Value(0.5, Token(54)), pointsto(x2C,x1V,x5H),assign(x0C,x1V,x2C,x4V),pointsto(x0C,x4V,x5H)),
		Rule(55, Value(0.5, Token(55)), pointsto(x0C,x3V,x5H),assign(x0C,x1V,x4C,x3V),pointsto(x4C,x1V,x5H)),
		Rule(56, Value(0.5, Token(56)), pointsto(x2C,x3V,x5H),assign(x0C,x1V,x2C,x3V),pointsto(x2C,x1V,x5H)),
		Rule(57, Value(0.5, Token(57)), pointsto(x0C,x1V,x5H),assign(x0C,x1V,x2C,x4V),pointsto(x2C,x4V,x5H)),
		Rule(58, Value(0.5, Token(58)), pointsto(x2C,x1V,x5H),assign(x0C,x1V,x2C,x4V),pointsto(x2C,x4V,x5H)),
		Rule(59, Value(0.5, Token(59)), pointsto(x0C,x1V,x2H),heappointsto(x5H,x4F,x2H),pointsto(x0C,x3V,x5H),store(x3V,x4F,x1V)),
		Rule(60, Value(0.5, Token(60)), pointsto(x0C,x1V,x2H),heappointsto(x5H,x4F,x2H),load(x3V,x4F,x1V),pointsto(x0C,x3V,x5H)),
		Rule(61, Value(0.5, Token(61)), heappointsto(x2H,x1F,x0H),heappointsto(x0H,x1F,x2H)),
		Rule(62, Value(0.5, Token(62)), heappointsto(x3H,x2F,x1H),heappointsto(x1H,x2F,x3H),points_initial(x0V,x1H)),
		Rule(63, Value(0.5, Token(63)), heappointsto(x1H,x3F,x2H),heappointsto(x2H,x3F,x1H),points_initial(x0V,x1H)),
		Rule(64, Value(0.5, Token(64)), heappointsto(x0H,x4F,x3H),heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
		Rule(65, Value(0.5, Token(65)), heappointsto(x0H,x4F,x3H),actual(x0H,x1Z,x2V),heappointsto(x3H,x4F,x0H)),
		Rule(66, Value(0.5, Token(66)), heappointsto(x4H,x1F,x3H),heappointsto(x3H,x1F,x4H),store(x0V,x1F,x2V)),
		Rule(67, Value(0.5, Token(67)), heappointsto(x4H,x1F,x3H),heappointsto(x3H,x1F,x4H),load(x0V,x1F,x2V)),
		Rule(68, Value(0.5, Token(68)), heappointsto(x0H,x1F,x4H),heappointsto(x0H,x1F,x2H),heappointsto(x0H,x3F,x4H)),
		Rule(69, Value(0.5, Token(69)), heappointsto(x2H,x1F,x4H),heappointsto(x0H,x1F,x2H),heappointsto(x0H,x3F,x4H)),
		Rule(70, Value(0.5, Token(70)), heappointsto(x2H,x3F,x4H),heappointsto(x0H,x1F,x2H),heappointsto(x0H,x3F,x4H)),
		Rule(71, Value(0.5, Token(71)), heappointsto(x0H,x1F,x3H),heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
		Rule(72, Value(0.5, Token(72)), heappointsto(x0H,x4F,x2H),heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
		Rule(73, Value(0.5, Token(73)), heappointsto(x2H,x1F,x0H),heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
		Rule(74, Value(0.5, Token(74)), heappointsto(x2H,x1F,x0H),heappointsto(x0H,x1F,x2H),pointsto(x3C,x4V,x0H)),
		Rule(75, Value(0.5, Token(75)), heappointsto(x2H,x4F,x0H),heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
		Rule(76, Value(0.5, Token(76)), heappointsto(x2H,x4F,x3H),heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
		Rule(77, Value(0.5, Token(77)), heappointsto(x3H,x1F,x0H),heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
		Rule(78, Value(0.5, Token(78)), heappointsto(x3H,x1F,x2H),heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
		Rule(79, Value(0.5, Token(79)), heappointsto(x3H,x4F,x2H),heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
		Rule(80, Value(0.5, Token(80)), heappointsto(x0H,x1F,x3H),heappointsto(x0H,x1F,x2H),heappointsto(x3H,x1F,x4H)),
		Rule(81, Value(0.5, Token(81)), heappointsto(x0H,x1F,x4H),heappointsto(x0H,x1F,x2H),heappointsto(x3H,x1F,x4H)),
		Rule(82, Value(0.5, Token(82)), heappointsto(x2H,x1F,x3H),heappointsto(x0H,x1F,x2H),heappointsto(x3H,x1F,x4H)),
		Rule(83, Value(0.5, Token(83)), heappointsto(x2H,x1F,x4H),heappointsto(x0H,x1F,x2H),heappointsto(x3H,x1F,x4H)),
		Rule(84, Value(0.5, Token(84)), heappointsto(x0H,x1F,x3H),heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x2H)),
		Rule(85, Value(0.5, Token(85)), heappointsto(x0H,x4F,x2H),heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x2H)),
		Rule(86, Value(0.5, Token(86)), heappointsto(x0H,x4F,x3H),heappointsto(x0H,x4F,x2H),heappointsto(x3H,x4F,x0H)),
		Rule(87, Value(0.5, Token(87)), heappointsto(x0H,x4F,x3H),heappointsto(x0H,x1F,x3H),heappointsto(x3H,x4F,x0H)),
		Rule(88, Value(0.5, Token(88)), heappointsto(x4H,x1F,x3H),heappointsto(x0H,x1F,x3H),heappointsto(x3H,x1F,x4H)),
		Rule(89, Value(0.5, Token(89)), heappointsto(x2H,x1F,x4H),heappointsto(x0H,x1F,x2H),heappointsto(x0H,x1F,x4H)),
		Rule(90, Value(0.5, Token(90)), heappointsto(x2H,x1F,x3H),heappointsto(x0H,x1F,x2H),heappointsto(x3H,x1F,x0H)),
		Rule(91, Value(0.5, Token(91)), heappointsto(x3H,x1F,x2H),heappointsto(x0H,x1F,x2H),heappointsto(x3H,x1F,x0H)),
		Rule(92, Value(0.5, Token(92)), heappointsto(x0H,x1F,x2H),pointsto(x5C,x3V,x0H),pointsto(x5C,x4V,x2H),store(x3V,x1F,x4V)),
		Rule(93, Value(0.5, Token(93)), heappointsto(x0H,x1F,x2H),load(x3V,x1F,x4V),pointsto(x5C,x3V,x0H),pointsto(x5C,x4V,x2H)),
	)
	val soupProg = Program("callsite_1Soup", soup)
	val evaluator = SeminaiveEvaluator(soupProg)
	override val expected: Set[Any] = Set(1, 8, 61, 93)
	override val maxVarCount: Int = 6
}
