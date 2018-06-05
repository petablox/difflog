package qd
package learner
import org.scalatest.{FunSuite, Ignore}
@Ignore
class Gendowncast_10 extends Problem {
	override val name = "downcast"
	val HSet = Range(0, 5).map(i => Atom(i)).toSet
	val H = Domain("H", HSet)
	val MSet = Range(0, 5).map(i => Atom(i)).toSet
	val M = Domain("M", MSet)
	val TSet = Range(0, 7).map(i => Atom(i)).toSet
	val T = Domain("T", TSet)
	val VSet = Range(0, 42).map(i => Atom(i)).toSet
	val V = Domain("V", VSet)
	val VH = Relation("VH", V,H)
	val McheckCastInst = Relation("McheckCastInst", M,V,T,V)
	val notSub = Relation("notSub", T,T)
	val HT = Relation("HT", H,T)
	val ptsVT = Relation("ptsVT", V,T)
	val unsafeDowncast = Relation("unsafeDowncast", V,T)
	val badCast = Relation("badCast", V,T)
	val reachableCast = Relation("reachableCast", T,V)
	val unsafeDowncastTuples = Set((34, 3),(29, 4),(0, 4),(1, 5),(2, 1),(2, 2),(3, 3),(3, 5),(4, 4),(5, 0),(5, 5),(6, 1),(6, 6),(9, 4),(11, 0),(11, 1),(12, 6),(13, 2),(13, 6),(15, 0),(15, 1),(15, 6),(16, 3),(16, 5),(17, 5),(18, 2),(19, 0),(19, 3),(20, 1),(20, 4),(21, 6),(24, 1),(26, 3),(28, 0),(28, 3),(28, 5),(29, 1),(30, 0),(31, 2),(32, 6),(33, 2),(33, 4),(34, 4),(38, 2),(39, 1),(40, 4),(40, 5),(40, 6)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val VHTuples = Set((0, 4),(0, 2),(0, 1),(0, 3),(32, 3),(16, 3),(8, 2),(40, 3),(24, 2),(36, 3),(20, 4),(12, 3),(28, 3),(2, 4),(2, 2),(34, 2),(34, 1),(34, 3),(18, 2),(18, 3),(10, 2),(26, 4),(26, 2),(6, 1),(38, 3),(22, 4),(1, 3),(33, 2),(17, 2),(17, 3),(9, 2),(41, 4),(41, 2),(41, 1),(41, 3),(25, 4),(25, 2),(5, 1),(37, 3),(21, 4),(13, 3),(29, 1),(29, 3),(35, 3),(19, 2),(19, 3),(11, 3),(27, 4),(27, 2),(7, 1),(39, 3),(23, 4),(15, 2)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val HTTuples = Set((4, 4),(2, 4),(1, 2),(3, 3)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val ptsVTTuples = Set((0, 4),(0, 2),(0, 3),(32, 3),(8, 4),(40, 3),(24, 4),(36, 3),(20, 4),(12, 3),(28, 3),(2, 4),(34, 4),(34, 2),(34, 3),(18, 3),(10, 4),(26, 4),(6, 2),(1, 3),(33, 4),(17, 4),(17, 3),(9, 4),(41, 4),(41, 2),(41, 3),(5, 2),(21, 4),(13, 3),(15, 4),(29, 3),(35, 3),(19, 4),(19, 3),(11, 3),(27, 4),(7, 2),(39, 3),(23, 4),(1, 1),(2, 1),(2, 5),(5, 3),(7, 0),(7, 1),(7, 6),(10, 2),(10, 5),(11, 1),(11, 6),(13, 5),(14, 2),(16, 1),(17, 6),(18, 1),(20, 3),(23, 3),(27, 2),(27, 6),(28, 2),(30, 4),(33, 6),(34, 1),(34, 6),(36, 1),(37, 1),(38, 2),(39, 6)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val badCastTuples = Set((0, 0),(6, 0),(5, 0),(29, 0),(7, 0),(0, 3),(34, 3),(6, 3),(41, 3),(5, 3),(29, 3),(7, 3),(0, 4),(34, 4),(6, 4),(41, 4),(5, 4),(29, 4),(34, 5),(6, 5),(41, 5),(5, 5),(29, 5),(7, 5),(32, 0),(16, 0),(40, 0),(36, 0),(12, 0),(28, 0),(18, 0),(1, 0),(17, 0),(37, 0),(13, 0),(35, 0),(19, 0),(11, 0),(39, 0),(16, 4),(40, 4),(36, 4),(12, 4),(18, 4),(38, 4),(1, 4),(17, 4),(37, 4),(35, 4),(19, 4),(11, 4),(32, 5),(16, 5),(40, 5),(36, 5),(12, 5),(28, 5),(18, 5),(38, 5),(1, 5),(17, 5),(35, 5),(19, 5),(11, 5),(8, 0),(24, 0),(20, 0),(2, 0),(10, 0),(26, 0),(22, 0),(33, 0),(9, 0),(25, 0),(21, 0),(27, 0),(23, 0),(15, 0),(8, 3),(24, 3),(20, 3),(2, 3),(18, 3),(10, 3),(26, 3),(22, 3),(33, 3),(17, 3),(9, 3),(25, 3),(21, 3),(19, 3),(27, 3),(23, 3),(15, 3),(8, 5),(24, 5),(2, 5),(10, 5),(26, 5),(22, 5),(33, 5),(9, 5),(25, 5),(21, 5),(27, 5),(23, 5),(15, 5),(0, 1),(1, 3),(2, 2),(4, 2),(5, 6),(6, 1),(11, 3),(13, 3),(13, 6),(14, 0),(14, 1),(14, 2),(22, 1),(26, 6),(30, 2),(30, 4),(31, 5),(33, 2),(34, 6),(35, 3),(35, 6),(37, 2),(39, 1),(40, 2)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val reachableCastTuples = Set((3, 34),(3, 38),(2, 17),(4, 29),(0, 12),(0, 32),(1, 12),(1, 14),(1, 24),(1, 32),(2, 6),(2, 9),(2, 10),(2, 16),(3, 23),(4, 1),(4, 15),(4, 19),(5, 12),(5, 17),(5, 20),(5, 37),(6, 1),(6, 7),(6, 11),(6, 12),(6, 14),(6, 19)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val McheckCastInstTuples = Set((4, 18, 2, 17),(4, 26, 4, 25),(4, 30, 4, 29),(4, 35, 3, 34),(4, 39, 3, 38)).map { case (x0,x1,x2,x3) => DTuple(Atom(x0),Atom(x1),Atom(x2),Atom(x3)) }
	val notSubTuples = Set((0, 0),(2, 0),(2, 3),(2, 4),(2, 5),(3, 0),(3, 4),(3, 5),(4, 0),(4, 3),(4, 5),(5, 0),(5, 2),(5, 3),(5, 4),(6, 0),(6, 2),(6, 3),(6, 4),(6, 5)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
override val edb = Config(
VH -> (Instance[FValue](VH) ++ VHTuples.map(t => t -> FValue.One).toMap),
McheckCastInst -> (Instance[FValue](McheckCastInst) ++ McheckCastInstTuples.map(t => t -> FValue.One).toMap),
notSub -> (Instance[FValue](notSub) ++ notSubTuples.map(t => t -> FValue.One).toMap),
HT -> (Instance[FValue](HT) ++ HTTuples.map(t => t -> FValue.One).toMap),
)
override val refOut = Config(
ptsVT -> (Instance[FValue](ptsVT) ++ ptsVTTuples.map(t => t -> FValue.One).toMap),
unsafeDowncast -> (Instance[FValue](unsafeDowncast) ++ unsafeDowncastTuples.map(t => t -> FValue.One).toMap),
badCast -> (Instance[FValue](badCast) ++ badCastTuples.map(t => t -> FValue.One).toMap),
reachableCast -> (Instance[FValue](reachableCast) ++ reachableCastTuples.map(t => t -> FValue.One).toMap),
)
	val x1V = Variable("x1V",V)
	val x1T = Variable("x1T",T)
	val x0V = Variable("x0V",V)
	val x0T = Variable("x0T",T)
	val x2T = Variable("x2T",T)
	val x2V = Variable("x2V",V)
	val x3T = Variable("x3T",T)
	val x3V = Variable("x3V",V)
	val x2M = Variable("x2M",M)
	val x4V = Variable("x4V",V)
	val x0H = Variable("x0H",H)
	val x1H = Variable("x1H",H)
	val x4T = Variable("x4T",T)
	val x0M = Variable("x0M",M)
	val soup = Set(
		Rule(0, FValue(0.5, Token(0)), badCast(x1V,x2T),McheckCastInst(x0M,x1V,x2T,x3V)),
		Rule(1, FValue(0.5, Token(1)), badCast(x3V,x2T),McheckCastInst(x0M,x1V,x2T,x3V)),
		Rule(2, FValue(0.5, Token(2)), badCast(x0V,x1T),ptsVT(x0V,x1T)),
		Rule(3, FValue(0.5, Token(3)), badCast(x0V,x1T),unsafeDowncast(x0V,x1T)),
		Rule(4, FValue(0.5, Token(4)), badCast(x1V,x0T),reachableCast(x0T,x1V)),
		Rule(5, FValue(0.5, Token(5)), badCast(x3V,x0T),McheckCastInst(x2M,x3V,x0T,x4V),reachableCast(x0T,x1V)),
		Rule(6, FValue(0.5, Token(6)), badCast(x3V,x0T),McheckCastInst(x2M,x3V,x0T,x4V),notSub(x0T,x1T)),
		Rule(7, FValue(0.5, Token(7)), badCast(x3V,x1T),McheckCastInst(x2M,x3V,x0T,x4V),notSub(x0T,x1T)),
		Rule(8, FValue(0.5, Token(8)), badCast(x4V,x1T),McheckCastInst(x2M,x3V,x0T,x4V),notSub(x0T,x1T)),
		Rule(9, FValue(0.5, Token(9)), badCast(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),badCast(x0V,x1T)),
		Rule(10, FValue(0.5, Token(10)), badCast(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x0V,x1T)),
		Rule(11, FValue(0.5, Token(11)), badCast(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x0V,x1T)),
		Rule(12, FValue(0.5, Token(12)), badCast(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),notSub(x0T,x1T)),
		Rule(13, FValue(0.5, Token(13)), badCast(x3V,x1T),HT(x0H,x1T),McheckCastInst(x2M,x3V,x1T,x4V)),
		Rule(14, FValue(0.5, Token(14)), badCast(x2V,x0T),ptsVT(x2V,x0T),reachableCast(x0T,x1V)),
		Rule(15, FValue(0.5, Token(15)), badCast(x2V,x0T),reachableCast(x0T,x1V),unsafeDowncast(x2V,x0T)),
		Rule(16, FValue(0.5, Token(16)), badCast(x2V,x0T),notSub(x0T,x1T),ptsVT(x2V,x0T)),
		Rule(17, FValue(0.5, Token(17)), badCast(x2V,x0T),notSub(x0T,x1T),unsafeDowncast(x2V,x0T)),
		Rule(18, FValue(0.5, Token(18)), badCast(x2V,x1T),badCast(x2V,x0T),notSub(x0T,x1T)),
		Rule(19, FValue(0.5, Token(19)), badCast(x2V,x1T),notSub(x0T,x1T),ptsVT(x2V,x0T)),
		Rule(20, FValue(0.5, Token(20)), badCast(x2V,x1T),notSub(x0T,x1T),unsafeDowncast(x2V,x0T)),
		Rule(21, FValue(0.5, Token(21)), badCast(x2V,x1T),HT(x0H,x1T),VH(x2V,x0H)),
		Rule(22, FValue(0.5, Token(22)), badCast(x2V,x0T),badCast(x2V,x1T),notSub(x0T,x1T)),
		Rule(23, FValue(0.5, Token(23)), badCast(x2V,x0T),notSub(x0T,x1T),ptsVT(x2V,x1T)),
		Rule(24, FValue(0.5, Token(24)), badCast(x2V,x0T),notSub(x0T,x1T),unsafeDowncast(x2V,x1T)),
		Rule(25, FValue(0.5, Token(25)), badCast(x2V,x1T),badCast(x0V,x1T),ptsVT(x2V,x1T)),
		Rule(26, FValue(0.5, Token(26)), badCast(x2V,x1T),badCast(x0V,x1T),unsafeDowncast(x2V,x1T)),
		Rule(27, FValue(0.5, Token(27)), badCast(x2V,x1T),ptsVT(x0V,x1T),unsafeDowncast(x2V,x1T)),
		Rule(28, FValue(0.5, Token(28)), badCast(x2V,x1T),ptsVT(x2V,x1T),unsafeDowncast(x0V,x1T)),
		Rule(29, FValue(0.5, Token(29)), badCast(x2V,x1T),notSub(x0T,x1T),ptsVT(x2V,x1T)),
		Rule(30, FValue(0.5, Token(30)), badCast(x2V,x1T),notSub(x0T,x1T),unsafeDowncast(x2V,x1T)),
		Rule(31, FValue(0.5, Token(31)), badCast(x2V,x1T),HT(x0H,x1T),ptsVT(x2V,x1T)),
		Rule(32, FValue(0.5, Token(32)), badCast(x2V,x1T),HT(x0H,x1T),unsafeDowncast(x2V,x1T)),
		Rule(33, FValue(0.5, Token(33)), badCast(x2V,x0T),notSub(x0T,x1T),reachableCast(x0T,x2V)),
		Rule(34, FValue(0.5, Token(34)), badCast(x2V,x1T),notSub(x0T,x1T),reachableCast(x0T,x2V)),
		Rule(35, FValue(0.5, Token(35)), badCast(x2V,x0T),notSub(x0T,x1T),reachableCast(x1T,x2V)),
		Rule(36, FValue(0.5, Token(36)), badCast(x2V,x1T),badCast(x0V,x1T),reachableCast(x1T,x2V)),
		Rule(37, FValue(0.5, Token(37)), badCast(x2V,x1T),ptsVT(x0V,x1T),reachableCast(x1T,x2V)),
		Rule(38, FValue(0.5, Token(38)), badCast(x2V,x1T),reachableCast(x1T,x2V),unsafeDowncast(x0V,x1T)),
		Rule(39, FValue(0.5, Token(39)), badCast(x2V,x1T),notSub(x0T,x1T),reachableCast(x1T,x2V)),
		Rule(40, FValue(0.5, Token(40)), badCast(x2V,x1T),HT(x0H,x1T),reachableCast(x1T,x2V)),
		Rule(41, FValue(0.5, Token(41)), badCast(x0V,x3T),McheckCastInst(x2M,x0V,x3T,x4V),badCast(x0V,x1T)),
		Rule(42, FValue(0.5, Token(42)), badCast(x0V,x3T),McheckCastInst(x2M,x0V,x3T,x4V),ptsVT(x0V,x1T)),
		Rule(43, FValue(0.5, Token(43)), badCast(x0V,x3T),McheckCastInst(x2M,x0V,x3T,x4V),unsafeDowncast(x0V,x1T)),
		Rule(44, FValue(0.5, Token(44)), badCast(x0V,x3T),McheckCastInst(x2M,x0V,x3T,x4V),VH(x0V,x1H)),
		Rule(45, FValue(0.5, Token(45)), badCast(x0V,x4T),McheckCastInst(x2M,x3V,x4T,x0V),badCast(x0V,x1T)),
		Rule(46, FValue(0.5, Token(46)), badCast(x0V,x4T),McheckCastInst(x2M,x3V,x4T,x0V),ptsVT(x0V,x1T)),
		Rule(47, FValue(0.5, Token(47)), badCast(x0V,x4T),McheckCastInst(x2M,x3V,x4T,x0V),unsafeDowncast(x0V,x1T)),
		Rule(48, FValue(0.5, Token(48)), badCast(x0V,x4T),McheckCastInst(x2M,x3V,x4T,x0V),VH(x0V,x1H)),
		Rule(49, FValue(0.5, Token(49)), badCast(x0V,x2T),badCast(x0V,x1T),reachableCast(x2T,x0V)),
		Rule(50, FValue(0.5, Token(50)), badCast(x0V,x2T),ptsVT(x0V,x1T),reachableCast(x2T,x0V)),
		Rule(51, FValue(0.5, Token(51)), badCast(x0V,x2T),reachableCast(x2T,x0V),unsafeDowncast(x0V,x1T)),
		Rule(52, FValue(0.5, Token(52)), badCast(x0V,x2T),VH(x0V,x1H),reachableCast(x2T,x0V)),
		Rule(53, FValue(0.5, Token(53)), badCast(x1V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),ptsVT(x1V,x4T)),
		Rule(54, FValue(0.5, Token(54)), badCast(x1V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),unsafeDowncast(x1V,x4T)),
		Rule(55, FValue(0.5, Token(55)), badCast(x3V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),badCast(x1V,x4T)),
		Rule(56, FValue(0.5, Token(56)), badCast(x3V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),ptsVT(x1V,x4T)),
		Rule(57, FValue(0.5, Token(57)), badCast(x3V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),unsafeDowncast(x1V,x4T)),
		Rule(58, FValue(0.5, Token(58)), badCast(x4V,x2T),McheckCastInst(x0M,x1V,x2T,x3V),ptsVT(x4V,x2T)),
		Rule(59, FValue(0.5, Token(59)), badCast(x4V,x2T),McheckCastInst(x0M,x1V,x2T,x3V),unsafeDowncast(x4V,x2T)),
		Rule(60, FValue(0.5, Token(60)), badCast(x1V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),badCast(x3V,x4T)),
		Rule(61, FValue(0.5, Token(61)), badCast(x1V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),ptsVT(x3V,x4T)),
		Rule(62, FValue(0.5, Token(62)), badCast(x1V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),unsafeDowncast(x3V,x4T)),
		Rule(63, FValue(0.5, Token(63)), badCast(x3V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),ptsVT(x3V,x4T)),
		Rule(64, FValue(0.5, Token(64)), badCast(x3V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),unsafeDowncast(x3V,x4T)),
		Rule(65, FValue(0.5, Token(65)), badCast(x1V,x2T),McheckCastInst(x0M,x1V,x2T,x3V),reachableCast(x4T,x1V)),
		Rule(66, FValue(0.5, Token(66)), badCast(x0V,x1T),ptsVT(x0V,x1T),reachableCast(x2T,x0V)),
		Rule(67, FValue(0.5, Token(67)), badCast(x0V,x1T),reachableCast(x2T,x0V),unsafeDowncast(x0V,x1T)),
		Rule(68, FValue(0.5, Token(68)), badCast(x3V,x0T),McheckCastInst(x2M,x3V,x0T,x4V),reachableCast(x0T,x4V)),
		Rule(69, FValue(0.5, Token(69)), badCast(x4V,x0T),McheckCastInst(x2M,x3V,x0T,x4V),reachableCast(x0T,x3V)),
		Rule(70, FValue(0.5, Token(70)), badCast(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x3V,x1T)),
		Rule(71, FValue(0.5, Token(71)), badCast(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x3V,x1T)),
		Rule(72, FValue(0.5, Token(72)), badCast(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),badCast(x4V,x1T)),
		Rule(73, FValue(0.5, Token(73)), badCast(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x4V,x1T)),
		Rule(74, FValue(0.5, Token(74)), badCast(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x4V,x1T)),
		Rule(75, FValue(0.5, Token(75)), badCast(x4V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),badCast(x3V,x1T)),
		Rule(76, FValue(0.5, Token(76)), badCast(x4V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x3V,x1T)),
		Rule(77, FValue(0.5, Token(77)), badCast(x4V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x3V,x1T)),
		Rule(78, FValue(0.5, Token(78)), badCast(x4V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x4V,x1T)),
		Rule(79, FValue(0.5, Token(79)), badCast(x4V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x4V,x1T)),
		Rule(80, FValue(0.5, Token(80)), badCast(x2V,x0T),ptsVT(x2V,x0T),reachableCast(x0T,x2V)),
		Rule(81, FValue(0.5, Token(81)), badCast(x2V,x0T),reachableCast(x0T,x2V),unsafeDowncast(x2V,x0T)),
		Rule(82, FValue(0.5, Token(82)), ptsVT(x1V,x2T),McheckCastInst(x0M,x1V,x2T,x3V)),
		Rule(83, FValue(0.5, Token(83)), ptsVT(x3V,x2T),McheckCastInst(x0M,x1V,x2T,x3V)),
		Rule(84, FValue(0.5, Token(84)), ptsVT(x0V,x1T),badCast(x0V,x1T)),
		Rule(85, FValue(0.5, Token(85)), ptsVT(x0V,x1T),unsafeDowncast(x0V,x1T)),
		Rule(86, FValue(0.5, Token(86)), ptsVT(x1V,x0T),reachableCast(x0T,x1V)),
		Rule(87, FValue(0.5, Token(87)), ptsVT(x3V,x0T),McheckCastInst(x2M,x3V,x0T,x4V),reachableCast(x0T,x1V)),
		Rule(88, FValue(0.5, Token(88)), ptsVT(x3V,x0T),McheckCastInst(x2M,x3V,x0T,x4V),notSub(x0T,x1T)),
		Rule(89, FValue(0.5, Token(89)), ptsVT(x3V,x1T),McheckCastInst(x2M,x3V,x0T,x4V),notSub(x0T,x1T)),
		Rule(90, FValue(0.5, Token(90)), ptsVT(x4V,x1T),McheckCastInst(x2M,x3V,x0T,x4V),notSub(x0T,x1T)),
		Rule(91, FValue(0.5, Token(91)), ptsVT(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),badCast(x0V,x1T)),
		Rule(92, FValue(0.5, Token(92)), ptsVT(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x0V,x1T)),
		Rule(93, FValue(0.5, Token(93)), ptsVT(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x0V,x1T)),
		Rule(94, FValue(0.5, Token(94)), ptsVT(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),notSub(x0T,x1T)),
		Rule(95, FValue(0.5, Token(95)), ptsVT(x3V,x1T),HT(x0H,x1T),McheckCastInst(x2M,x3V,x1T,x4V)),
		Rule(96, FValue(0.5, Token(96)), ptsVT(x2V,x0T),badCast(x2V,x0T),reachableCast(x0T,x1V)),
		Rule(97, FValue(0.5, Token(97)), ptsVT(x2V,x0T),reachableCast(x0T,x1V),unsafeDowncast(x2V,x0T)),
		Rule(98, FValue(0.5, Token(98)), ptsVT(x2V,x0T),badCast(x2V,x0T),notSub(x0T,x1T)),
		Rule(99, FValue(0.5, Token(99)), ptsVT(x2V,x0T),notSub(x0T,x1T),unsafeDowncast(x2V,x0T)),
		Rule(100, FValue(0.5, Token(100)), ptsVT(x2V,x1T),badCast(x2V,x0T),notSub(x0T,x1T)),
		Rule(101, FValue(0.5, Token(101)), ptsVT(x2V,x1T),notSub(x0T,x1T),ptsVT(x2V,x0T)),
		Rule(102, FValue(0.5, Token(102)), ptsVT(x2V,x1T),notSub(x0T,x1T),unsafeDowncast(x2V,x0T)),
		Rule(103, FValue(0.5, Token(103)), ptsVT(x2V,x1T),HT(x0H,x1T),VH(x2V,x0H)),
		Rule(104, FValue(0.5, Token(104)), ptsVT(x2V,x0T),badCast(x2V,x1T),notSub(x0T,x1T)),
		Rule(105, FValue(0.5, Token(105)), ptsVT(x2V,x0T),notSub(x0T,x1T),ptsVT(x2V,x1T)),
		Rule(106, FValue(0.5, Token(106)), ptsVT(x2V,x0T),notSub(x0T,x1T),unsafeDowncast(x2V,x1T)),
		Rule(107, FValue(0.5, Token(107)), ptsVT(x2V,x1T),badCast(x0V,x1T),unsafeDowncast(x2V,x1T)),
		Rule(108, FValue(0.5, Token(108)), ptsVT(x2V,x1T),badCast(x2V,x1T),ptsVT(x0V,x1T)),
		Rule(109, FValue(0.5, Token(109)), ptsVT(x2V,x1T),ptsVT(x0V,x1T),unsafeDowncast(x2V,x1T)),
		Rule(110, FValue(0.5, Token(110)), ptsVT(x2V,x1T),badCast(x2V,x1T),unsafeDowncast(x0V,x1T)),
		Rule(111, FValue(0.5, Token(111)), ptsVT(x2V,x1T),badCast(x2V,x1T),notSub(x0T,x1T)),
		Rule(112, FValue(0.5, Token(112)), ptsVT(x2V,x1T),notSub(x0T,x1T),unsafeDowncast(x2V,x1T)),
		Rule(113, FValue(0.5, Token(113)), ptsVT(x2V,x1T),HT(x0H,x1T),badCast(x2V,x1T)),
		Rule(114, FValue(0.5, Token(114)), ptsVT(x2V,x1T),HT(x0H,x1T),unsafeDowncast(x2V,x1T)),
		Rule(115, FValue(0.5, Token(115)), ptsVT(x2V,x0T),notSub(x0T,x1T),reachableCast(x0T,x2V)),
		Rule(116, FValue(0.5, Token(116)), ptsVT(x2V,x1T),notSub(x0T,x1T),reachableCast(x0T,x2V)),
		Rule(117, FValue(0.5, Token(117)), ptsVT(x2V,x0T),notSub(x0T,x1T),reachableCast(x1T,x2V)),
		Rule(118, FValue(0.5, Token(118)), ptsVT(x2V,x1T),badCast(x0V,x1T),reachableCast(x1T,x2V)),
		Rule(119, FValue(0.5, Token(119)), ptsVT(x2V,x1T),ptsVT(x0V,x1T),reachableCast(x1T,x2V)),
		Rule(120, FValue(0.5, Token(120)), ptsVT(x2V,x1T),reachableCast(x1T,x2V),unsafeDowncast(x0V,x1T)),
		Rule(121, FValue(0.5, Token(121)), ptsVT(x2V,x1T),notSub(x0T,x1T),reachableCast(x1T,x2V)),
		Rule(122, FValue(0.5, Token(122)), ptsVT(x2V,x1T),HT(x0H,x1T),reachableCast(x1T,x2V)),
		Rule(123, FValue(0.5, Token(123)), ptsVT(x0V,x3T),McheckCastInst(x2M,x0V,x3T,x4V),badCast(x0V,x1T)),
		Rule(124, FValue(0.5, Token(124)), ptsVT(x0V,x3T),McheckCastInst(x2M,x0V,x3T,x4V),ptsVT(x0V,x1T)),
		Rule(125, FValue(0.5, Token(125)), ptsVT(x0V,x3T),McheckCastInst(x2M,x0V,x3T,x4V),unsafeDowncast(x0V,x1T)),
		Rule(126, FValue(0.5, Token(126)), ptsVT(x0V,x3T),McheckCastInst(x2M,x0V,x3T,x4V),VH(x0V,x1H)),
		Rule(127, FValue(0.5, Token(127)), ptsVT(x0V,x4T),McheckCastInst(x2M,x3V,x4T,x0V),badCast(x0V,x1T)),
		Rule(128, FValue(0.5, Token(128)), ptsVT(x0V,x4T),McheckCastInst(x2M,x3V,x4T,x0V),ptsVT(x0V,x1T)),
		Rule(129, FValue(0.5, Token(129)), ptsVT(x0V,x4T),McheckCastInst(x2M,x3V,x4T,x0V),unsafeDowncast(x0V,x1T)),
		Rule(130, FValue(0.5, Token(130)), ptsVT(x0V,x4T),McheckCastInst(x2M,x3V,x4T,x0V),VH(x0V,x1H)),
		Rule(131, FValue(0.5, Token(131)), ptsVT(x0V,x2T),badCast(x0V,x1T),reachableCast(x2T,x0V)),
		Rule(132, FValue(0.5, Token(132)), ptsVT(x0V,x2T),ptsVT(x0V,x1T),reachableCast(x2T,x0V)),
		Rule(133, FValue(0.5, Token(133)), ptsVT(x0V,x2T),reachableCast(x2T,x0V),unsafeDowncast(x0V,x1T)),
		Rule(134, FValue(0.5, Token(134)), ptsVT(x0V,x2T),VH(x0V,x1H),reachableCast(x2T,x0V)),
		Rule(135, FValue(0.5, Token(135)), ptsVT(x1V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),badCast(x1V,x4T)),
		Rule(136, FValue(0.5, Token(136)), ptsVT(x1V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),unsafeDowncast(x1V,x4T)),
		Rule(137, FValue(0.5, Token(137)), ptsVT(x3V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),badCast(x1V,x4T)),
		Rule(138, FValue(0.5, Token(138)), ptsVT(x3V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),ptsVT(x1V,x4T)),
		Rule(139, FValue(0.5, Token(139)), ptsVT(x3V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),unsafeDowncast(x1V,x4T)),
		Rule(140, FValue(0.5, Token(140)), ptsVT(x4V,x2T),McheckCastInst(x0M,x1V,x2T,x3V),badCast(x4V,x2T)),
		Rule(141, FValue(0.5, Token(141)), ptsVT(x4V,x2T),McheckCastInst(x0M,x1V,x2T,x3V),unsafeDowncast(x4V,x2T)),
		Rule(142, FValue(0.5, Token(142)), ptsVT(x1V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),badCast(x3V,x4T)),
		Rule(143, FValue(0.5, Token(143)), ptsVT(x1V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),ptsVT(x3V,x4T)),
		Rule(144, FValue(0.5, Token(144)), ptsVT(x1V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),unsafeDowncast(x3V,x4T)),
		Rule(145, FValue(0.5, Token(145)), ptsVT(x3V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),badCast(x3V,x4T)),
		Rule(146, FValue(0.5, Token(146)), ptsVT(x3V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),unsafeDowncast(x3V,x4T)),
		Rule(147, FValue(0.5, Token(147)), ptsVT(x1V,x2T),McheckCastInst(x0M,x1V,x2T,x3V),reachableCast(x4T,x1V)),
		Rule(148, FValue(0.5, Token(148)), ptsVT(x0V,x1T),badCast(x0V,x1T),reachableCast(x2T,x0V)),
		Rule(149, FValue(0.5, Token(149)), ptsVT(x0V,x1T),reachableCast(x2T,x0V),unsafeDowncast(x0V,x1T)),
		Rule(150, FValue(0.5, Token(150)), ptsVT(x3V,x0T),McheckCastInst(x2M,x3V,x0T,x4V),reachableCast(x0T,x4V)),
		Rule(151, FValue(0.5, Token(151)), ptsVT(x4V,x0T),McheckCastInst(x2M,x3V,x0T,x4V),reachableCast(x0T,x3V)),
		Rule(152, FValue(0.5, Token(152)), ptsVT(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),badCast(x3V,x1T)),
		Rule(153, FValue(0.5, Token(153)), ptsVT(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x3V,x1T)),
		Rule(154, FValue(0.5, Token(154)), ptsVT(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),badCast(x4V,x1T)),
		Rule(155, FValue(0.5, Token(155)), ptsVT(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x4V,x1T)),
		Rule(156, FValue(0.5, Token(156)), ptsVT(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x4V,x1T)),
		Rule(157, FValue(0.5, Token(157)), ptsVT(x4V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),badCast(x3V,x1T)),
		Rule(158, FValue(0.5, Token(158)), ptsVT(x4V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x3V,x1T)),
		Rule(159, FValue(0.5, Token(159)), ptsVT(x4V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x3V,x1T)),
		Rule(160, FValue(0.5, Token(160)), ptsVT(x4V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),badCast(x4V,x1T)),
		Rule(161, FValue(0.5, Token(161)), ptsVT(x4V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x4V,x1T)),
		Rule(162, FValue(0.5, Token(162)), ptsVT(x2V,x0T),badCast(x2V,x0T),reachableCast(x0T,x2V)),
		Rule(163, FValue(0.5, Token(163)), ptsVT(x2V,x0T),reachableCast(x0T,x2V),unsafeDowncast(x2V,x0T)),
		Rule(164, FValue(0.5, Token(164)), reachableCast(x2T,x1V),McheckCastInst(x0M,x1V,x2T,x3V)),
		Rule(165, FValue(0.5, Token(165)), reachableCast(x2T,x3V),McheckCastInst(x0M,x1V,x2T,x3V)),
		Rule(166, FValue(0.5, Token(166)), reachableCast(x1T,x0V),badCast(x0V,x1T)),
		Rule(167, FValue(0.5, Token(167)), reachableCast(x1T,x0V),ptsVT(x0V,x1T)),
		Rule(168, FValue(0.5, Token(168)), reachableCast(x1T,x0V),unsafeDowncast(x0V,x1T)),
		Rule(169, FValue(0.5, Token(169)), reachableCast(x0T,x3V),McheckCastInst(x2M,x3V,x0T,x4V),reachableCast(x0T,x1V)),
		Rule(170, FValue(0.5, Token(170)), reachableCast(x0T,x3V),McheckCastInst(x2M,x3V,x0T,x4V),notSub(x0T,x1T)),
		Rule(171, FValue(0.5, Token(171)), reachableCast(x0T,x4V),McheckCastInst(x2M,x3V,x0T,x4V),reachableCast(x0T,x1V)),
		Rule(172, FValue(0.5, Token(172)), reachableCast(x0T,x4V),McheckCastInst(x2M,x3V,x0T,x4V),notSub(x0T,x1T)),
		Rule(173, FValue(0.5, Token(173)), reachableCast(x0T,x3V),McheckCastInst(x2M,x3V,x1T,x4V),notSub(x0T,x1T)),
		Rule(174, FValue(0.5, Token(174)), reachableCast(x0T,x4V),McheckCastInst(x2M,x3V,x1T,x4V),notSub(x0T,x1T)),
		Rule(175, FValue(0.5, Token(175)), reachableCast(x1T,x4V),McheckCastInst(x2M,x3V,x1T,x4V),badCast(x0V,x1T)),
		Rule(176, FValue(0.5, Token(176)), reachableCast(x1T,x4V),McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x0V,x1T)),
		Rule(177, FValue(0.5, Token(177)), reachableCast(x1T,x4V),McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x0V,x1T)),
		Rule(178, FValue(0.5, Token(178)), reachableCast(x1T,x4V),McheckCastInst(x2M,x3V,x1T,x4V),notSub(x0T,x1T)),
		Rule(179, FValue(0.5, Token(179)), reachableCast(x1T,x4V),HT(x0H,x1T),McheckCastInst(x2M,x3V,x1T,x4V)),
		Rule(180, FValue(0.5, Token(180)), reachableCast(x0T,x2V),badCast(x2V,x0T),reachableCast(x0T,x1V)),
		Rule(181, FValue(0.5, Token(181)), reachableCast(x0T,x2V),ptsVT(x2V,x0T),reachableCast(x0T,x1V)),
		Rule(182, FValue(0.5, Token(182)), reachableCast(x0T,x2V),reachableCast(x0T,x1V),unsafeDowncast(x2V,x0T)),
		Rule(183, FValue(0.5, Token(183)), reachableCast(x0T,x2V),badCast(x2V,x0T),notSub(x0T,x1T)),
		Rule(184, FValue(0.5, Token(184)), reachableCast(x0T,x2V),notSub(x0T,x1T),ptsVT(x2V,x0T)),
		Rule(185, FValue(0.5, Token(185)), reachableCast(x0T,x2V),notSub(x0T,x1T),unsafeDowncast(x2V,x0T)),
		Rule(186, FValue(0.5, Token(186)), reachableCast(x1T,x2V),badCast(x2V,x0T),notSub(x0T,x1T)),
		Rule(187, FValue(0.5, Token(187)), reachableCast(x1T,x2V),notSub(x0T,x1T),ptsVT(x2V,x0T)),
		Rule(188, FValue(0.5, Token(188)), reachableCast(x1T,x2V),notSub(x0T,x1T),unsafeDowncast(x2V,x0T)),
		Rule(189, FValue(0.5, Token(189)), reachableCast(x1T,x2V),HT(x0H,x1T),VH(x2V,x0H)),
		Rule(190, FValue(0.5, Token(190)), reachableCast(x0T,x2V),badCast(x2V,x1T),notSub(x0T,x1T)),
		Rule(191, FValue(0.5, Token(191)), reachableCast(x0T,x2V),notSub(x0T,x1T),ptsVT(x2V,x1T)),
		Rule(192, FValue(0.5, Token(192)), reachableCast(x0T,x2V),notSub(x0T,x1T),unsafeDowncast(x2V,x1T)),
		Rule(193, FValue(0.5, Token(193)), reachableCast(x1T,x2V),badCast(x0V,x1T),ptsVT(x2V,x1T)),
		Rule(194, FValue(0.5, Token(194)), reachableCast(x1T,x2V),badCast(x0V,x1T),unsafeDowncast(x2V,x1T)),
		Rule(195, FValue(0.5, Token(195)), reachableCast(x1T,x2V),badCast(x2V,x1T),ptsVT(x0V,x1T)),
		Rule(196, FValue(0.5, Token(196)), reachableCast(x1T,x2V),ptsVT(x0V,x1T),unsafeDowncast(x2V,x1T)),
		Rule(197, FValue(0.5, Token(197)), reachableCast(x1T,x2V),badCast(x2V,x1T),unsafeDowncast(x0V,x1T)),
		Rule(198, FValue(0.5, Token(198)), reachableCast(x1T,x2V),ptsVT(x2V,x1T),unsafeDowncast(x0V,x1T)),
		Rule(199, FValue(0.5, Token(199)), reachableCast(x1T,x2V),badCast(x2V,x1T),notSub(x0T,x1T)),
		Rule(200, FValue(0.5, Token(200)), reachableCast(x1T,x2V),notSub(x0T,x1T),ptsVT(x2V,x1T)),
		Rule(201, FValue(0.5, Token(201)), reachableCast(x1T,x2V),notSub(x0T,x1T),unsafeDowncast(x2V,x1T)),
		Rule(202, FValue(0.5, Token(202)), reachableCast(x1T,x2V),HT(x0H,x1T),badCast(x2V,x1T)),
		Rule(203, FValue(0.5, Token(203)), reachableCast(x1T,x2V),HT(x0H,x1T),ptsVT(x2V,x1T)),
		Rule(204, FValue(0.5, Token(204)), reachableCast(x1T,x2V),HT(x0H,x1T),unsafeDowncast(x2V,x1T)),
		Rule(205, FValue(0.5, Token(205)), reachableCast(x1T,x2V),notSub(x0T,x1T),reachableCast(x0T,x2V)),
		Rule(206, FValue(0.5, Token(206)), reachableCast(x0T,x2V),notSub(x0T,x1T),reachableCast(x1T,x2V)),
		Rule(207, FValue(0.5, Token(207)), reachableCast(x4T,x0V),McheckCastInst(x2M,x3V,x4T,x0V),badCast(x0V,x1T)),
		Rule(208, FValue(0.5, Token(208)), reachableCast(x4T,x0V),McheckCastInst(x2M,x3V,x4T,x0V),ptsVT(x0V,x1T)),
		Rule(209, FValue(0.5, Token(209)), reachableCast(x4T,x0V),McheckCastInst(x2M,x3V,x4T,x0V),unsafeDowncast(x0V,x1T)),
		Rule(210, FValue(0.5, Token(210)), reachableCast(x4T,x0V),McheckCastInst(x2M,x3V,x4T,x0V),VH(x0V,x1H)),
		Rule(211, FValue(0.5, Token(211)), reachableCast(x2T,x1V),McheckCastInst(x0M,x1V,x2T,x3V),reachableCast(x4T,x1V)),
		Rule(212, FValue(0.5, Token(212)), reachableCast(x4T,x3V),McheckCastInst(x0M,x1V,x2T,x3V),reachableCast(x4T,x1V)),
		Rule(213, FValue(0.5, Token(213)), reachableCast(x2T,x3V),McheckCastInst(x0M,x1V,x2T,x3V),reachableCast(x4T,x3V)),
		Rule(214, FValue(0.5, Token(214)), reachableCast(x4T,x1V),McheckCastInst(x0M,x1V,x2T,x3V),reachableCast(x4T,x3V)),
		Rule(215, FValue(0.5, Token(215)), reachableCast(x1T,x0V),badCast(x0V,x1T),reachableCast(x2T,x0V)),
		Rule(216, FValue(0.5, Token(216)), reachableCast(x1T,x0V),ptsVT(x0V,x1T),reachableCast(x2T,x0V)),
		Rule(217, FValue(0.5, Token(217)), reachableCast(x1T,x0V),reachableCast(x2T,x0V),unsafeDowncast(x0V,x1T)),
		Rule(218, FValue(0.5, Token(218)), reachableCast(x0T,x3V),McheckCastInst(x2M,x3V,x0T,x4V),reachableCast(x0T,x4V)),
		Rule(219, FValue(0.5, Token(219)), reachableCast(x0T,x4V),McheckCastInst(x2M,x3V,x0T,x4V),reachableCast(x0T,x3V)),
		Rule(220, FValue(0.5, Token(220)), reachableCast(x1T,x3V),McheckCastInst(x2M,x3V,x1T,x4V),badCast(x4V,x1T)),
		Rule(221, FValue(0.5, Token(221)), reachableCast(x1T,x3V),McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x4V,x1T)),
		Rule(222, FValue(0.5, Token(222)), reachableCast(x1T,x3V),McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x4V,x1T)),
		Rule(223, FValue(0.5, Token(223)), reachableCast(x1T,x4V),McheckCastInst(x2M,x3V,x1T,x4V),badCast(x3V,x1T)),
		Rule(224, FValue(0.5, Token(224)), reachableCast(x1T,x4V),McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x3V,x1T)),
		Rule(225, FValue(0.5, Token(225)), reachableCast(x1T,x4V),McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x3V,x1T)),
		Rule(226, FValue(0.5, Token(226)), unsafeDowncast(x1V,x2T),McheckCastInst(x0M,x1V,x2T,x3V)),
		Rule(227, FValue(0.5, Token(227)), unsafeDowncast(x3V,x2T),McheckCastInst(x0M,x1V,x2T,x3V)),
		Rule(228, FValue(0.5, Token(228)), unsafeDowncast(x0V,x1T),badCast(x0V,x1T)),
		Rule(229, FValue(0.5, Token(229)), unsafeDowncast(x0V,x1T),ptsVT(x0V,x1T)),
		Rule(230, FValue(0.5, Token(230)), unsafeDowncast(x1V,x0T),reachableCast(x0T,x1V)),
		Rule(231, FValue(0.5, Token(231)), unsafeDowncast(x3V,x0T),McheckCastInst(x2M,x3V,x0T,x4V),reachableCast(x0T,x1V)),
		Rule(232, FValue(0.5, Token(232)), unsafeDowncast(x3V,x0T),McheckCastInst(x2M,x3V,x0T,x4V),notSub(x0T,x1T)),
		Rule(233, FValue(0.5, Token(233)), unsafeDowncast(x3V,x1T),McheckCastInst(x2M,x3V,x0T,x4V),notSub(x0T,x1T)),
		Rule(234, FValue(0.5, Token(234)), unsafeDowncast(x4V,x1T),McheckCastInst(x2M,x3V,x0T,x4V),notSub(x0T,x1T)),
		Rule(235, FValue(0.5, Token(235)), unsafeDowncast(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),badCast(x0V,x1T)),
		Rule(236, FValue(0.5, Token(236)), unsafeDowncast(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x0V,x1T)),
		Rule(237, FValue(0.5, Token(237)), unsafeDowncast(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x0V,x1T)),
		Rule(238, FValue(0.5, Token(238)), unsafeDowncast(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),notSub(x0T,x1T)),
		Rule(239, FValue(0.5, Token(239)), unsafeDowncast(x3V,x1T),HT(x0H,x1T),McheckCastInst(x2M,x3V,x1T,x4V)),
		Rule(240, FValue(0.5, Token(240)), unsafeDowncast(x2V,x0T),badCast(x2V,x0T),reachableCast(x0T,x1V)),
		Rule(241, FValue(0.5, Token(241)), unsafeDowncast(x2V,x0T),ptsVT(x2V,x0T),reachableCast(x0T,x1V)),
		Rule(242, FValue(0.5, Token(242)), unsafeDowncast(x2V,x0T),badCast(x2V,x0T),notSub(x0T,x1T)),
		Rule(243, FValue(0.5, Token(243)), unsafeDowncast(x2V,x0T),notSub(x0T,x1T),ptsVT(x2V,x0T)),
		Rule(244, FValue(0.5, Token(244)), unsafeDowncast(x2V,x1T),badCast(x2V,x0T),notSub(x0T,x1T)),
		Rule(245, FValue(0.5, Token(245)), unsafeDowncast(x2V,x1T),notSub(x0T,x1T),ptsVT(x2V,x0T)),
		Rule(246, FValue(0.5, Token(246)), unsafeDowncast(x2V,x1T),notSub(x0T,x1T),unsafeDowncast(x2V,x0T)),
		Rule(247, FValue(0.5, Token(247)), unsafeDowncast(x2V,x1T),HT(x0H,x1T),VH(x2V,x0H)),
		Rule(248, FValue(0.5, Token(248)), unsafeDowncast(x2V,x0T),badCast(x2V,x1T),notSub(x0T,x1T)),
		Rule(249, FValue(0.5, Token(249)), unsafeDowncast(x2V,x0T),notSub(x0T,x1T),ptsVT(x2V,x1T)),
		Rule(250, FValue(0.5, Token(250)), unsafeDowncast(x2V,x0T),notSub(x0T,x1T),unsafeDowncast(x2V,x1T)),
		Rule(251, FValue(0.5, Token(251)), unsafeDowncast(x2V,x1T),badCast(x0V,x1T),ptsVT(x2V,x1T)),
		Rule(252, FValue(0.5, Token(252)), unsafeDowncast(x2V,x1T),badCast(x2V,x1T),ptsVT(x0V,x1T)),
		Rule(253, FValue(0.5, Token(253)), unsafeDowncast(x2V,x1T),badCast(x2V,x1T),unsafeDowncast(x0V,x1T)),
		Rule(254, FValue(0.5, Token(254)), unsafeDowncast(x2V,x1T),ptsVT(x2V,x1T),unsafeDowncast(x0V,x1T)),
		Rule(255, FValue(0.5, Token(255)), unsafeDowncast(x2V,x1T),badCast(x2V,x1T),notSub(x0T,x1T)),
		Rule(256, FValue(0.5, Token(256)), unsafeDowncast(x2V,x1T),notSub(x0T,x1T),ptsVT(x2V,x1T)),
		Rule(257, FValue(0.5, Token(257)), unsafeDowncast(x2V,x1T),HT(x0H,x1T),badCast(x2V,x1T)),
		Rule(258, FValue(0.5, Token(258)), unsafeDowncast(x2V,x1T),HT(x0H,x1T),ptsVT(x2V,x1T)),
		Rule(259, FValue(0.5, Token(259)), unsafeDowncast(x2V,x0T),notSub(x0T,x1T),reachableCast(x0T,x2V)),
		Rule(260, FValue(0.5, Token(260)), unsafeDowncast(x2V,x1T),notSub(x0T,x1T),reachableCast(x0T,x2V)),
		Rule(261, FValue(0.5, Token(261)), unsafeDowncast(x2V,x0T),notSub(x0T,x1T),reachableCast(x1T,x2V)),
		Rule(262, FValue(0.5, Token(262)), unsafeDowncast(x2V,x1T),badCast(x0V,x1T),reachableCast(x1T,x2V)),
		Rule(263, FValue(0.5, Token(263)), unsafeDowncast(x2V,x1T),ptsVT(x0V,x1T),reachableCast(x1T,x2V)),
		Rule(264, FValue(0.5, Token(264)), unsafeDowncast(x2V,x1T),reachableCast(x1T,x2V),unsafeDowncast(x0V,x1T)),
		Rule(265, FValue(0.5, Token(265)), unsafeDowncast(x2V,x1T),notSub(x0T,x1T),reachableCast(x1T,x2V)),
		Rule(266, FValue(0.5, Token(266)), unsafeDowncast(x2V,x1T),HT(x0H,x1T),reachableCast(x1T,x2V)),
		Rule(267, FValue(0.5, Token(267)), unsafeDowncast(x0V,x3T),McheckCastInst(x2M,x0V,x3T,x4V),badCast(x0V,x1T)),
		Rule(268, FValue(0.5, Token(268)), unsafeDowncast(x0V,x3T),McheckCastInst(x2M,x0V,x3T,x4V),ptsVT(x0V,x1T)),
		Rule(269, FValue(0.5, Token(269)), unsafeDowncast(x0V,x3T),McheckCastInst(x2M,x0V,x3T,x4V),unsafeDowncast(x0V,x1T)),
		Rule(270, FValue(0.5, Token(270)), unsafeDowncast(x0V,x3T),McheckCastInst(x2M,x0V,x3T,x4V),VH(x0V,x1H)),
		Rule(271, FValue(0.5, Token(271)), unsafeDowncast(x0V,x4T),McheckCastInst(x2M,x3V,x4T,x0V),badCast(x0V,x1T)),
		Rule(272, FValue(0.5, Token(272)), unsafeDowncast(x0V,x4T),McheckCastInst(x2M,x3V,x4T,x0V),ptsVT(x0V,x1T)),
		Rule(273, FValue(0.5, Token(273)), unsafeDowncast(x0V,x4T),McheckCastInst(x2M,x3V,x4T,x0V),unsafeDowncast(x0V,x1T)),
		Rule(274, FValue(0.5, Token(274)), unsafeDowncast(x0V,x4T),McheckCastInst(x2M,x3V,x4T,x0V),VH(x0V,x1H)),
		Rule(275, FValue(0.5, Token(275)), unsafeDowncast(x0V,x2T),badCast(x0V,x1T),reachableCast(x2T,x0V)),
		Rule(276, FValue(0.5, Token(276)), unsafeDowncast(x0V,x2T),ptsVT(x0V,x1T),reachableCast(x2T,x0V)),
		Rule(277, FValue(0.5, Token(277)), unsafeDowncast(x0V,x2T),reachableCast(x2T,x0V),unsafeDowncast(x0V,x1T)),
		Rule(278, FValue(0.5, Token(278)), unsafeDowncast(x0V,x2T),VH(x0V,x1H),reachableCast(x2T,x0V)),
		Rule(279, FValue(0.5, Token(279)), unsafeDowncast(x1V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),badCast(x1V,x4T)),
		Rule(280, FValue(0.5, Token(280)), unsafeDowncast(x1V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),ptsVT(x1V,x4T)),
		Rule(281, FValue(0.5, Token(281)), unsafeDowncast(x3V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),badCast(x1V,x4T)),
		Rule(282, FValue(0.5, Token(282)), unsafeDowncast(x3V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),ptsVT(x1V,x4T)),
		Rule(283, FValue(0.5, Token(283)), unsafeDowncast(x3V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),unsafeDowncast(x1V,x4T)),
		Rule(284, FValue(0.5, Token(284)), unsafeDowncast(x4V,x2T),McheckCastInst(x0M,x1V,x2T,x3V),badCast(x4V,x2T)),
		Rule(285, FValue(0.5, Token(285)), unsafeDowncast(x4V,x2T),McheckCastInst(x0M,x1V,x2T,x3V),ptsVT(x4V,x2T)),
		Rule(286, FValue(0.5, Token(286)), unsafeDowncast(x1V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),badCast(x3V,x4T)),
		Rule(287, FValue(0.5, Token(287)), unsafeDowncast(x1V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),ptsVT(x3V,x4T)),
		Rule(288, FValue(0.5, Token(288)), unsafeDowncast(x1V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),unsafeDowncast(x3V,x4T)),
		Rule(289, FValue(0.5, Token(289)), unsafeDowncast(x3V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),badCast(x3V,x4T)),
		Rule(290, FValue(0.5, Token(290)), unsafeDowncast(x3V,x4T),McheckCastInst(x0M,x1V,x2T,x3V),ptsVT(x3V,x4T)),
		Rule(291, FValue(0.5, Token(291)), unsafeDowncast(x1V,x2T),McheckCastInst(x0M,x1V,x2T,x3V),reachableCast(x4T,x1V)),
		Rule(292, FValue(0.5, Token(292)), unsafeDowncast(x0V,x1T),badCast(x0V,x1T),reachableCast(x2T,x0V)),
		Rule(293, FValue(0.5, Token(293)), unsafeDowncast(x0V,x1T),ptsVT(x0V,x1T),reachableCast(x2T,x0V)),
		Rule(294, FValue(0.5, Token(294)), unsafeDowncast(x3V,x0T),McheckCastInst(x2M,x3V,x0T,x4V),reachableCast(x0T,x4V)),
		Rule(295, FValue(0.5, Token(295)), unsafeDowncast(x4V,x0T),McheckCastInst(x2M,x3V,x0T,x4V),reachableCast(x0T,x3V)),
		Rule(296, FValue(0.5, Token(296)), unsafeDowncast(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),badCast(x3V,x1T)),
		Rule(297, FValue(0.5, Token(297)), unsafeDowncast(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x3V,x1T)),
		Rule(298, FValue(0.5, Token(298)), unsafeDowncast(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),badCast(x4V,x1T)),
		Rule(299, FValue(0.5, Token(299)), unsafeDowncast(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x4V,x1T)),
		Rule(300, FValue(0.5, Token(300)), unsafeDowncast(x3V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x4V,x1T)),
		Rule(301, FValue(0.5, Token(301)), unsafeDowncast(x4V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),badCast(x3V,x1T)),
		Rule(302, FValue(0.5, Token(302)), unsafeDowncast(x4V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x3V,x1T)),
		Rule(303, FValue(0.5, Token(303)), unsafeDowncast(x4V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x3V,x1T)),
		Rule(304, FValue(0.5, Token(304)), unsafeDowncast(x4V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),badCast(x4V,x1T)),
		Rule(305, FValue(0.5, Token(305)), unsafeDowncast(x4V,x1T),McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x4V,x1T)),
		Rule(306, FValue(0.5, Token(306)), unsafeDowncast(x2V,x0T),badCast(x2V,x0T),reachableCast(x0T,x2V)),
		Rule(307, FValue(0.5, Token(307)), unsafeDowncast(x2V,x0T),ptsVT(x2V,x0T),reachableCast(x0T,x2V)),
	)
	val soupProg = Program("downcastSoup", soup)
	val evaluator = SeminaiveEvaluator(soupProg)

	override val expected: Set[Any] = Set(20, 104, 166, 307)
	override val maxVarCount: Int = 4
}

