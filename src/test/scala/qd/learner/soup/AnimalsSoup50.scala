package qd
package learner
import org.scalatest.{FunSuite, Ignore}

class AnimalsSoup50 extends Problem {
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
  val fish = Relation("fish", A,A)
  val reptile = Relation("reptile", A)
  val bird = Relation("bird", A,A)
  val noneTuples = Set((3)).map { case (x0) => DTuple(Atom(x0)) }
  val homeothermicTuples = Set((0),(1),(2),(3),(13),(14),(15),(16)).map { case (x0) => DTuple(Atom(x0)) }
  val scalesTuples = Set((1)).map { case (x0) => DTuple(Atom(x0)) }
  val fishTuples = Set((4),(5),(6),(7)).map { case (x0) => DTuple(Atom(x0)) }
  val feathersTuples = Set((0)).map { case (x0) => DTuple(Atom(x0)) }
  val has_eggsTuples = Set((2),(4),(5),(6),(7),(8),(9),(10),(11),(12),(13),(14),(15)).map { case (x0) => DTuple(Atom(x0)) }
  val hairTuples = Set((2)).map { case (x0) => DTuple(Atom(x0)) }
  val has_coveringTuples = Set((0, 2),(1, 3),(2, 2),(3, 2),(4, 3),(5, 3),(6, 3),(7, 3),(8, 1),(9, 1),(10, 1),(11, 1),(12, 1),(13, 0),(14, 0),(15, 0)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
  val has_gillsTuples = Set((4),(5),(6),(7)).map { case (x0) => DTuple(Atom(x0)) }
  val mammalTuples = Set((0),(1),(2),(3)).map { case (x0) => DTuple(Atom(x0)) }
  val reptileTuples = Set((8),(9),(10),(11),(12)).map { case (x0) => DTuple(Atom(x0)) }
  val birdTuples = Set((13),(14),(15)).map { case (x0) => DTuple(Atom(x0)) }
  val has_milkTuples = Set((0),(1),(2),(3),(16)).map { case (x0) => DTuple(Atom(x0)) }
  override val edb = Config(
    feathers -> (Instance[FValue](feathers) ++ feathersTuples.map(t => t -> FValue.One).toMap),
    scales -> (Instance[FValue](scales) ++ scalesTuples.map(t => t -> FValue.One).toMap),
    hair -> (Instance[FValue](hair) ++ hairTuples.map(t => t -> FValue.One).toMap),
    none -> (Instance[FValue](none) ++ noneTuples.map(t => t -> FValue.One).toMap),
    has_covering -> (Instance[FValue](has_covering) ++ has_coveringTuples.map(t => t -> FValue.One).toMap),
    has_milk -> (Instance[FValue](has_milk) ++ has_milkTuples.map(t => t -> FValue.One).toMap),
    homeothermic -> (Instance[FValue](homeothermic) ++ homeothermicTuples.map(t => t -> FValue.One).toMap),
    has_eggs -> (Instance[FValue](has_eggs) ++ has_eggsTuples.map(t => t -> FValue.One).toMap),
    has_gills -> (Instance[FValue](has_gills) ++ has_gillsTuples.map(t => t -> FValue.One).toMap),
    )
  override val refOut = Config(
    mammal -> (Instance[FValue](mammal) ++ mammalTuples.map(t => t -> FValue.One).toMap),
    fish -> (Instance[FValue](fish) ++ fishTuples.map(t => t -> FValue.One).toMap),
    reptile -> (Instance[FValue](reptile) ++ reptileTuples.map(t => t -> FValue.One).toMap),
    bird -> (Instance[FValue](bird) ++ birdTuples.map(t => t -> FValue.One).toMap),
    )
  val x0A = Variable("x0A",A)
  val x1C = Variable("x1C",C)
  val soup = Set(
    Rule(0, FValue(0.5, Token(0)), bird(x0A),reptile(x0A)),
    Rule(1, FValue(0.5, Token(1)), bird(x0A),mammal(x0A)),
    Rule(2, FValue(0.5, Token(2)), bird(x0A),fish(x0A)),
    Rule(3, FValue(0.5, Token(3)), bird(x0A),homeothermic(x0A)),
    Rule(5, FValue(0.5, Token(5)), bird(x0A),has_milk(x0A)),
    Rule(6, FValue(0.5, Token(6)), bird(x0A),has_gills(x0A)),
    Rule(8, FValue(0.5, Token(8)), bird(x0A),has_covering(x0A,x1C),reptile(x0A)),
    Rule(9, FValue(0.5, Token(9)), bird(x0A),has_covering(x0A,x1C),mammal(x0A)),
    Rule(10, FValue(0.5, Token(10)), bird(x0A),fish(x0A),has_covering(x0A,x1C)),
    Rule(15, FValue(0.5, Token(15)), bird(x0A),feathers(x1C),has_covering(x0A,x1C)),
    Rule(25, FValue(0.5, Token(25)), fish(x0A),has_gills(x0A)),
    Rule(26, FValue(0.5, Token(26)), fish(x0A),has_covering(x0A,x1C)),
    Rule(27, FValue(0.5, Token(27)), fish(x0A),has_covering(x0A,x1C),reptile(x0A)),
    Rule(32, FValue(0.5, Token(32)), fish(x0A),has_covering(x0A,x1C),has_milk(x0A)),
    Rule(34, FValue(0.5, Token(34)), fish(x0A),feathers(x1C),has_covering(x0A,x1C)),
    Rule(36, FValue(0.5, Token(36)), fish(x0A),hair(x1C),has_covering(x0A,x1C)),
    Rule(41, FValue(0.5, Token(41)), mammal(x0A),homeothermic(x0A)),
    Rule(46, FValue(0.5, Token(46)), mammal(x0A),has_covering(x0A,x1C),reptile(x0A)),
    Rule(48, FValue(0.5, Token(48)), mammal(x0A),bird(x0A),has_covering(x0A,x1C)),
    Rule(50, FValue(0.5, Token(50)), mammal(x0A),has_covering(x0A,x1C),has_eggs(x0A)),
    Rule(51, FValue(0.5, Token(51)), mammal(x0A),has_covering(x0A,x1C),has_milk(x0A)),
    Rule(53, FValue(0.5, Token(53)), mammal(x0A),feathers(x1C),has_covering(x0A,x1C)),
    Rule(55, FValue(0.5, Token(55)), mammal(x0A),hair(x1C),has_covering(x0A,x1C)),
    Rule(56, FValue(0.5, Token(56)), mammal(x0A),has_covering(x0A,x1C),none(x1C)),
    Rule(58, FValue(0.5, Token(58)), reptile(x0A),fish(x0A)),
    Rule(59, FValue(0.5, Token(59)), reptile(x0A),bird(x0A)),
    Rule(61, FValue(0.5, Token(61)), reptile(x0A),has_eggs(x0A)),
    Rule(62, FValue(0.5, Token(62)), reptile(x0A),has_milk(x0A)),
    Rule(64, FValue(0.5, Token(64)), reptile(x0A),has_covering(x0A,x1C)),
    Rule(65, FValue(0.5, Token(65)), reptile(x0A),has_covering(x0A,x1C),mammal(x0A)),
    Rule(67, FValue(0.5, Token(67)), reptile(x0A),bird(x0A),has_covering(x0A,x1C)),
    Rule(70, FValue(0.5, Token(70)), reptile(x0A),has_covering(x0A,x1C),has_milk(x0A)),
    Rule(71, FValue(0.5, Token(71)), reptile(x0A),has_covering(x0A,x1C),has_gills(x0A)),
    Rule(72, FValue(0.5, Token(72)), reptile(x0A),feathers(x1C),has_covering(x0A,x1C)),
    Rule(73, FValue(0.5, Token(73)), reptile(x0A),has_covering(x0A,x1C),scales(x1C)),
    Rule(74, FValue(0.5, Token(74)), reptile(x0A),hair(x1C),has_covering(x0A,x1C)),
    Rule(75, FValue(0.5, Token(75)), reptile(x0A),has_covering(x0A,x1C),none(x1C)),
    )

  override val expected = Set(15,25,51,73)
  override val maxVarCount: Int = 20
}
