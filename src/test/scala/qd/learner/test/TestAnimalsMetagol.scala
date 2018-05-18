package qd
package learner
import org.scalatest.{FunSuite, Ignore}

class TestAnimalsMetagol extends Problem {
  override val name = "animals"
  val ASet = Range(0, 64).map(i => Atom(i)).toSet
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
    Rule(1, Value(1.0, Token(1)), bird(x0A),homeothermic(x0A)),
    Rule(2, Value(1.0, Token(2)), fish(x0A),has_gills(x0A)),
    Rule(3, Value(1.0, Token(3)), mammal(x0A),homeothermic(x0A)),
    Rule(4, Value(1.0, Token(4)), reptile(x0A),has_eggs(x0A))
    )

  override val expected = Set(15,25,51,73)
  override val maxVarCount: Int = 20
}
