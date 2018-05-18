package qd
package learner
import org.scalatest.{FunSuite, Ignore}

class Genknightsmove_0 extends Problem {
	override val name = "knightsmove"
	val VSet = Range(0, 64).map(i => Atom(i)).toSet
	val V = Domain("V", VSet)
	val east = Relation("east", V,V)
	val sw = Relation("sw", V,V)
	val se = Relation("se", V,V)
	val south = Relation("south", V,V)
	val knight = Relation("knight", V,V)
	val knightTuples = Set((0, 17),(8, 25),(16, 33),(24, 41),(32, 49),(40, 57),(1, 18),(9, 26),(17, 34),(25, 42),(33, 50),(41, 58),(2, 19),(10, 27),(18, 35),(26, 43),(34, 51),(42, 59),(3, 20),(11, 28),(19, 36),(27, 44),(35, 52),(43, 60),(4, 21),(12, 29),(20, 37),(28, 45),(36, 53),(44, 61),(5, 22),(13, 30),(21, 38),(29, 46),(37, 54),(45, 62),(6, 23),(14, 31),(22, 39),(30, 47),(38, 55),(46, 63)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val eastTuples = Set((0, 1),(8, 9),(16, 17),(24, 25),(32, 33),(40, 41),(48, 49),(56, 57),(1, 2),(9, 10),(17, 18),(25, 26),(33, 34),(41, 42),(49, 50),(57, 58),(2, 3),(10, 11),(18, 19),(26, 27),(34, 35),(42, 43),(50, 51),(58, 59),(3, 4),(11, 12),(19, 20),(27, 28),(35, 36),(43, 44),(51, 52),(59, 60),(4, 5),(12, 13),(20, 21),(28, 29),(36, 37),(44, 45),(52, 53),(60, 61),(5, 6),(13, 14),(21, 22),(29, 30),(37, 38),(45, 46),(53, 54),(61, 62),(6, 7),(14, 15),(22, 23),(30, 31),(38, 39),(46, 47),(54, 55),(62, 63)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val swTuples = Set((1, 8),(9, 16),(17, 24),(25, 32),(33, 40),(41, 48),(49, 56),(2, 9),(10, 17),(18, 25),(26, 33),(34, 41),(42, 49),(50, 57),(3, 10),(11, 18),(19, 26),(27, 34),(35, 42),(43, 50),(51, 58),(4, 11),(12, 19),(20, 27),(28, 35),(36, 43),(44, 51),(52, 59),(5, 12),(13, 20),(21, 28),(29, 36),(37, 44),(45, 52),(53, 60),(6, 13),(14, 21),(22, 29),(30, 37),(38, 45),(46, 53),(54, 61),(7, 14),(15, 22),(23, 30),(31, 38),(39, 46),(47, 54),(55, 62)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val southTuples = Set((0, 8),(8, 16),(16, 24),(24, 32),(32, 40),(40, 48),(48, 56),(1, 9),(9, 17),(17, 25),(25, 33),(33, 41),(41, 49),(49, 57),(2, 10),(10, 18),(18, 26),(26, 34),(34, 42),(42, 50),(50, 58),(3, 11),(11, 19),(19, 27),(27, 35),(35, 43),(43, 51),(51, 59),(4, 12),(12, 20),(20, 28),(28, 36),(36, 44),(44, 52),(52, 60),(5, 13),(13, 21),(21, 29),(29, 37),(37, 45),(45, 53),(53, 61),(6, 14),(14, 22),(22, 30),(30, 38),(38, 46),(46, 54),(54, 62),(7, 15),(15, 23),(23, 31),(31, 39),(39, 47),(47, 55),(55, 63)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val seTuples = Set((0, 9),(8, 17),(16, 25),(24, 33),(32, 41),(40, 49),(48, 57),(1, 10),(9, 18),(17, 26),(25, 34),(33, 42),(41, 50),(49, 58),(2, 11),(10, 19),(18, 27),(26, 35),(34, 43),(42, 51),(50, 59),(3, 12),(11, 20),(19, 28),(27, 36),(35, 44),(43, 52),(51, 60),(4, 13),(12, 21),(20, 29),(28, 37),(36, 45),(44, 53),(52, 61),(5, 14),(13, 22),(21, 30),(29, 38),(37, 46),(45, 54),(53, 62),(6, 15),(14, 23),(22, 31),(30, 39),(38, 47),(46, 55),(54, 63)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	override val edb = Config(
		east -> (Instance(east) ++ eastTuples.map(t => t -> One).toMap),
		sw -> (Instance(sw) ++ swTuples.map(t => t -> One).toMap),
		se -> (Instance(se) ++ seTuples.map(t => t -> One).toMap),
		south -> (Instance(south) ++ southTuples.map(t => t -> One).toMap),
	)
	override val refOut = Config(
		knight -> (Instance(knight) ++ knightTuples.map(t => t -> One).toMap),
	)
	val y = Variable("y",V)
	val x = Variable("x",V)
	val z = Variable("z",V)
	val soup_pre = Set(
		Rule(1, Value(0.500000, Token(1)), knight(x,y),east(x,z),south(z,y)),
		Rule(2, Value(0.500000, Token(2)), knight(x,y),east(z,x),south(z,y)),
		Rule(3, Value(0.500000, Token(3)), knight(x,y),east(x,z),south(y,z)),
		Rule(4, Value(0.500000, Token(4)), knight(x,y),east(z,x),south(y,z)),
		Rule(5, Value(0.500000, Token(5)), knight(x,y),east(x,z),east(z,y)),
		Rule(6, Value(0.500000, Token(6)), knight(x,y),east(z,x),east(z,y)),
		Rule(7, Value(0.500000, Token(7)), knight(x,y),east(x,z),east(y,z)),
		Rule(8, Value(0.500000, Token(8)), knight(x,y),east(z,x),east(y,z)),
		Rule(9, Value(0.500000, Token(9)), knight(x,y),east(x,z),se(z,y)),
		Rule(10, Value(0.500000, Token(10)), knight(x,y),east(z,x),se(z,y)),
		Rule(11, Value(0.500000, Token(11)), knight(x,y),east(x,z),se(y,z)),
		Rule(12, Value(0.500000, Token(12)), knight(x,y),east(z,x),se(y,z)),
		Rule(13, Value(0.500000, Token(13)), knight(x,y),east(x,z),sw(z,y)),
		Rule(14, Value(0.500000, Token(14)), knight(x,y),east(z,x),sw(z,y)),
		Rule(15, Value(0.500000, Token(15)), knight(x,y),east(x,z),sw(y,z)),
		Rule(16, Value(0.500000, Token(16)), knight(x,y),east(z,x),sw(y,z)),
		Rule(17, Value(0.500000, Token(17)), knight(x,y),south(x,z),south(z,y)),
		Rule(18, Value(0.500000, Token(18)), knight(x,y),south(z,x),south(z,y)),
		Rule(19, Value(0.500000, Token(19)), knight(x,y),south(x,z),south(y,z)),
		Rule(20, Value(0.500000, Token(20)), knight(x,y),south(z,x),south(y,z)),
		Rule(21, Value(0.500000, Token(21)), knight(x,y),south(x,z),se(z,y)),
		Rule(22, Value(0.500000, Token(22)), knight(x,y),south(z,x),se(z,y)),
		Rule(23, Value(0.500000, Token(23)), knight(x,y),south(x,z),se(y,z)),
		Rule(24, Value(0.500000, Token(24)), knight(x,y),south(z,x),se(y,z)),
		Rule(25, Value(0.500000, Token(25)), knight(x,y),south(x,z),sw(z,y)),
		Rule(26, Value(0.500000, Token(26)), knight(x,y),south(z,x),sw(z,y)),
		Rule(27, Value(0.500000, Token(27)), knight(x,y),south(x,z),sw(y,z)),
		Rule(28, Value(0.500000, Token(28)), knight(x,y),south(z,x),sw(y,z)),
		Rule(29, Value(0.500000, Token(29)), knight(x,y),se(x,z),se(z,y)),
		Rule(30, Value(0.500000, Token(30)), knight(x,y),se(z,x),se(z,y)),
		Rule(31, Value(0.500000, Token(31)), knight(x,y),se(x,z),se(y,z)),
		Rule(32, Value(0.500000, Token(32)), knight(x,y),se(z,x),se(y,z)),
		Rule(33, Value(0.500000, Token(33)), knight(x,y),se(x,z),sw(z,y)),
		Rule(34, Value(0.500000, Token(34)), knight(x,y),se(z,x),sw(z,y)),
		Rule(35, Value(0.500000, Token(35)), knight(x,y),se(x,z),sw(y,z)),
		Rule(36, Value(0.500000, Token(36)), knight(x,y),se(z,x),sw(y,z)),
		Rule(37, Value(0.500000, Token(37)), knight(x,y),sw(x,z),sw(z,y)),
		Rule(38, Value(0.500000, Token(38)), knight(x,y),sw(z,x),sw(z,y)),
		Rule(39, Value(0.500000, Token(39)), knight(x,y),sw(x,z),sw(y,z)),
		Rule(40, Value(0.500000, Token(40)), knight(x,y),sw(z,x),sw(y,z)),
	)

	val expected = Set(21)
	val maxVarCount = 3

  val usefulTokens= Set(21)
  val soup =
    soup_pre.map(r => Rule(r.name, Value(1.0, r.coeff.prov), r.head, r.body)).
      filter(r => usefulTokens.contains(r.name.asInstanceOf[Int]))

  val soupProg = Program("knightsmoveSoup", soup)
  val evaluator = SeminaiveEvaluator(soupProg)

}
