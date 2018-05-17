package qd
package learner
import org.scalatest.{FunSuite, Ignore}

class TestAnimals extends Problem {
  override val name = "animals"
  val ASet = Range(0, 21).map(i => Atom(i)).toSet
  val A = Domain("A", ASet)
  val CSet = Range(0, 4).map(i => Atom(i)).toSet
  val C = Domain("C", CSet)
  val feathers = Relation("feathers", C)
  val scales = Relation("scales", C)
  val hair = Relation("hair", C)
  val none = Relation("none", C)
  val has_covering = Relation("has_covering", A,C)
  val has_milk = Relation("has_milk", A)
  val homeothermic = Relation("homeothermic", A)
  val has_eggs = Relation("has_eggs", A)
  val has_gills = Relation("has_gills", A)
  val mammal = Relation("mammal", A)
  val fish = Relation("fish", A)
  val reptile = Relation("reptile", A)
  val bird = Relation("bird", A)
  val noneTuples = Set((3)).map { case (x0) => DTuple(Atom(x0)) }
  val homeothermicTuples = Set(58, 53, 0, 59, 28, 40, 35, 62, 9, 10, 42, 6, 7, 18, 45, 41, 14).map { case (x0) => DTuple(Atom(x0)) }
  val scalesTuples = Set((1)).map { case (x0) => DTuple(Atom(x0)) }
  val fishTuples = Set(36, 53, 32, 59, 33, 1, 34, 61, 40, 3, 62, 30, 63, 37, 11, 17, 24, 46, 25).map { case (x0) => DTuple(Atom(x0)) }
  val feathersTuples = Set((0)).map { case (x0) => DTuple(Atom(x0)) }
  val has_eggsTuples = Set(15, 53, 40, 42, 2, 29, 8, 37, 41, 10, 5, 48, 44, 18, 45, 60).map { case (x0) => DTuple(Atom(x0)) }
  val hairTuples = Set((2)).map { case (x0) => DTuple(Atom(x0)) }
  val has_coveringTuples = Set((50, 3), (47, 2), (3, 0), (61, 3), (8, 0), (53, 3), (42, 2), (56, 3), (27, 1), (28, 1), (14, 0),
    (34, 2), (37, 2), (32, 2), (18, 1), (4, 0), (9, 0), (52, 3), (29, 1), (62, 3), (21, 1), (43, 2), (15, 0), (59, 3), (41, 2),
    (38, 2), (49, 3), (5, 0), (24, 1), (10, 0), (31, 1), (39, 2), (33, 2), (44, 2), (30, 1), (0, 0), (58, 3), (48, 3), (20, 1),
    (6, 0), (17, 1), (11, 0), (51, 3), (23, 1), (45, 2), (1, 0), (57, 3), (54, 3), (55, 3), (35, 2), (46, 2), (7, 0), (40, 2),
    (26, 1), (12, 0), (63, 3), (60, 3), (16, 1), (2, 0), (36, 2),
    (22, 1), (19, 1), (13, 0), (25, 1)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
  val has_gillsTuples = Set(53, 32, 59, 33, 1, 62, 34, 61, 40, 3, 30, 36, 63, 37, 11, 17, 24, 46, 25).map { case (x0) => DTuple(Atom(x0)) }
  val mammalTuples = Set(21, 32, 34, 61, 13, 24, 15, 53, 16, 45, 56, 58, 22, 8, 62, 39, 14, 0, 2, 40, 42, 44, 57).map { case (x0) => DTuple(Atom(x0)) }
  val reptileTuples = Set(26, 21, 27, 22, 28, 23, 29, 30, 31, 16, 17, 18, 24, 19, 25, 20).map { case (x0) => DTuple(Atom(x0)) }
  val birdTuples = Set(15, 0, 1, 2, 8, 3, 9, 4, 10, 5, 11, 6, 12, 7, 13, 14).map { case (x0) => DTuple(Atom(x0)) }
  val has_milkTuples = Set(21, 32, 34, 61, 13, 24, 15, 53, 16, 45, 56, 58, 22, 8, 62, 39, 14, 0, 2, 40, 42, 44, 57).map { case (x0) => DTuple(Atom(x0)) }
  override val edb = Config(
    feathers -> (Instance(feathers) ++ feathersTuples.map(t => t -> One).toMap),
    scales -> (Instance(scales) ++ scalesTuples.map(t => t -> One).toMap),
    hair -> (Instance(hair) ++ hairTuples.map(t => t -> One).toMap),
    none -> (Instance(none) ++ noneTuples.map(t => t -> One).toMap),
    has_covering -> (Instance(has_covering) ++ has_coveringTuples.map(t => t -> One).toMap),
    has_milk -> (Instance(has_milk) ++ has_milkTuples.map(t => t -> One).toMap),
    homeothermic -> (Instance(homeothermic) ++ homeothermicTuples.map(t => t -> One).toMap),
    has_eggs -> (Instance(has_eggs) ++ has_eggsTuples.map(t => t -> One).toMap),
    has_gills -> (Instance(has_gills) ++ has_gillsTuples.map(t => t -> One).toMap),
    )
  override val refOut = Config(
    mammal -> (Instance(mammal) ++ mammalTuples.map(t => t -> One).toMap),
    fish -> (Instance(fish) ++ fishTuples.map(t => t -> One).toMap),
    reptile -> (Instance(reptile) ++ reptileTuples.map(t => t -> One).toMap),
    bird -> (Instance(bird) ++ birdTuples.map(t => t -> One).toMap),
    )
  val x0A = Variable("x0A",A)
  val x1C = Variable("x1C",C)
  val soup = Set(
		Rule(1, Value(0.010000, Token(1)), bird(x0A),reptile(x0A)),
		Rule(2, Value(0.010000, Token(2)), bird(x0A),mammal(x0A)),
		Rule(3, Value(0.010000, Token(3)), bird(x0A),fish(x0A)),
		Rule(4, Value(0.010000, Token(4)), bird(x0A),homeothermic(x0A)),
		Rule(5, Value(0.010000, Token(5)), bird(x0A),has_eggs(x0A)),
		Rule(6, Value(0.010000, Token(6)), bird(x0A),has_milk(x0A)),
		Rule(7, Value(0.010000, Token(7)), bird(x0A),has_gills(x0A)),
		Rule(8, Value(0.010000, Token(8)), bird(x0A),has_covering(x0A,x1C)),
		Rule(9, Value(0.010000, Token(9)), bird(x0A),has_covering(x0A,x1C),reptile(x0A)),
		Rule(10, Value(0.010000, Token(10)), bird(x0A),has_covering(x0A,x1C),mammal(x0A)),
		Rule(11, Value(0.010000, Token(11)), bird(x0A),fish(x0A),has_covering(x0A,x1C)),
		Rule(12, Value(0.010000, Token(12)), bird(x0A),has_covering(x0A,x1C),homeothermic(x0A)),
		Rule(13, Value(0.010000, Token(13)), bird(x0A),has_covering(x0A,x1C),has_eggs(x0A)),
		Rule(14, Value(0.010000, Token(14)), bird(x0A),has_covering(x0A,x1C),has_milk(x0A)),
		Rule(15, Value(0.010000, Token(15)), bird(x0A),has_covering(x0A,x1C),has_gills(x0A)),
		Rule(16, Value(0.990000, Token(16)), bird(x0A),feathers(x1C),has_covering(x0A,x1C)),
		Rule(17, Value(0.010000, Token(17)), bird(x0A),has_covering(x0A,x1C),scales(x1C)),
		Rule(18, Value(0.010000, Token(18)), bird(x0A),hair(x1C),has_covering(x0A,x1C)),
		Rule(19, Value(0.010000, Token(19)), bird(x0A),has_covering(x0A,x1C),none(x1C)),
		Rule(20, Value(0.010000, Token(20)), fish(x0A),reptile(x0A)),
		Rule(21, Value(0.010000, Token(21)), fish(x0A),mammal(x0A)),
		Rule(22, Value(0.010000, Token(22)), fish(x0A),bird(x0A)),
		Rule(23, Value(0.010000, Token(23)), fish(x0A),homeothermic(x0A)),
		Rule(24, Value(0.010000, Token(24)), fish(x0A),has_eggs(x0A)),
		Rule(25, Value(0.010000, Token(25)), fish(x0A),has_milk(x0A)),
		Rule(26, Value(0.990000, Token(26)), fish(x0A),has_gills(x0A)),
		Rule(27, Value(0.010000, Token(27)), fish(x0A),has_covering(x0A,x1C)),
		Rule(28, Value(0.010000, Token(28)), fish(x0A),has_covering(x0A,x1C),reptile(x0A)),
		Rule(29, Value(0.010000, Token(29)), fish(x0A),has_covering(x0A,x1C),mammal(x0A)),
		Rule(30, Value(0.010000, Token(30)), fish(x0A),bird(x0A),has_covering(x0A,x1C)),
		Rule(31, Value(0.010000, Token(31)), fish(x0A),has_covering(x0A,x1C),homeothermic(x0A)),
		Rule(32, Value(0.010000, Token(32)), fish(x0A),has_covering(x0A,x1C),has_eggs(x0A)),
		Rule(33, Value(0.010000, Token(33)), fish(x0A),has_covering(x0A,x1C),has_milk(x0A)),
		Rule(34, Value(0.219931, Token(34)), fish(x0A),has_covering(x0A,x1C),has_gills(x0A)),
		Rule(35, Value(0.010000, Token(35)), fish(x0A),feathers(x1C),has_covering(x0A,x1C)),
		Rule(36, Value(0.010000, Token(36)), fish(x0A),has_covering(x0A,x1C),scales(x1C)),
		Rule(37, Value(0.010000, Token(37)), fish(x0A),hair(x1C),has_covering(x0A,x1C)),
		Rule(38, Value(0.010000, Token(38)), fish(x0A),has_covering(x0A,x1C),none(x1C)),
		Rule(39, Value(0.010000, Token(39)), mammal(x0A),reptile(x0A)),
		Rule(40, Value(0.010000, Token(40)), mammal(x0A),fish(x0A)),
		Rule(41, Value(0.010000, Token(41)), mammal(x0A),bird(x0A)),
		Rule(42, Value(0.010000, Token(42)), mammal(x0A),homeothermic(x0A)),
		Rule(43, Value(0.010000, Token(43)), mammal(x0A),has_eggs(x0A)),
		Rule(44, Value(0.010000, Token(44)), mammal(x0A),has_milk(x0A)),
		Rule(45, Value(0.010000, Token(45)), mammal(x0A),has_gills(x0A)),
		Rule(46, Value(0.010000, Token(46)), mammal(x0A),has_covering(x0A,x1C)),
		Rule(47, Value(0.010000, Token(47)), mammal(x0A),has_covering(x0A,x1C),reptile(x0A)),
		Rule(48, Value(0.010000, Token(48)), mammal(x0A),fish(x0A),has_covering(x0A,x1C)),
		Rule(49, Value(0.010000, Token(49)), mammal(x0A),bird(x0A),has_covering(x0A,x1C)),
		Rule(50, Value(0.010000, Token(50)), mammal(x0A),has_covering(x0A,x1C),homeothermic(x0A)),
		Rule(51, Value(0.010000, Token(51)), mammal(x0A),has_covering(x0A,x1C),has_eggs(x0A)),
		Rule(52, Value(0.990000, Token(52)), mammal(x0A),has_covering(x0A,x1C),has_milk(x0A)),
		Rule(53, Value(0.010000, Token(53)), mammal(x0A),has_covering(x0A,x1C),has_gills(x0A)),
		Rule(54, Value(0.010000, Token(54)), mammal(x0A),feathers(x1C),has_covering(x0A,x1C)),
		Rule(55, Value(0.010000, Token(55)), mammal(x0A),has_covering(x0A,x1C),scales(x1C)),
		Rule(56, Value(0.619098, Token(56)), mammal(x0A),hair(x1C),has_covering(x0A,x1C)),
		Rule(57, Value(0.010000, Token(57)), mammal(x0A),has_covering(x0A,x1C),none(x1C)),
		Rule(58, Value(0.010000, Token(58)), reptile(x0A),mammal(x0A)),
		Rule(59, Value(0.002216, Token(59)), reptile(x0A),fish(x0A)),
		Rule(60, Value(0.010000, Token(60)), reptile(x0A),bird(x0A)),
		Rule(61, Value(0.010000, Token(61)), reptile(x0A),homeothermic(x0A)),
		Rule(62, Value(0.010000, Token(62)), reptile(x0A),has_eggs(x0A)),
		Rule(63, Value(0.010000, Token(63)), reptile(x0A),has_milk(x0A)),
		Rule(64, Value(0.010000, Token(64)), reptile(x0A),has_gills(x0A)),
		Rule(65, Value(0.010000, Token(65)), reptile(x0A),has_covering(x0A,x1C)),
		Rule(66, Value(0.010000, Token(66)), reptile(x0A),has_covering(x0A,x1C),mammal(x0A)),
		Rule(67, Value(0.010000, Token(67)), reptile(x0A),fish(x0A),has_covering(x0A,x1C)),
		Rule(68, Value(0.004938, Token(68)), reptile(x0A),bird(x0A),has_covering(x0A,x1C)),
		Rule(69, Value(0.010000, Token(69)), reptile(x0A),has_covering(x0A,x1C),homeothermic(x0A)),
		Rule(70, Value(0.010000, Token(70)), reptile(x0A),has_covering(x0A,x1C),has_eggs(x0A)),
		Rule(71, Value(0.010000, Token(71)), reptile(x0A),has_covering(x0A,x1C),has_milk(x0A)),
		Rule(72, Value(0.004328, Token(72)), reptile(x0A),has_covering(x0A,x1C),has_gills(x0A)),
		Rule(73, Value(0.010000, Token(73)), reptile(x0A),feathers(x1C),has_covering(x0A,x1C)),
		Rule(74, Value(0.990000, Token(74)), reptile(x0A),has_covering(x0A,x1C),scales(x1C)),
		Rule(75, Value(0.010000, Token(75)), reptile(x0A),hair(x1C),has_covering(x0A,x1C)),
		Rule(76, Value(0.010000, Token(76)), reptile(x0A),has_covering(x0A,x1C),none(x1C)),
    )

  override val expected = Set(15,25,51,73)
  override val maxVarCount: Int = 20
}
