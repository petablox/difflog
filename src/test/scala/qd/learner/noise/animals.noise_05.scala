package qd
package learner
import org.scalatest.{FunSuite, Ignore}

class Genanimals_5 extends Problem {
	override val name = "animals"
	val ASet = Range(0, 21).map(i => Atom(i)).toSet
	val A = Domain("A", ASet)
	val CSet = Range(0, 4).map(i => Atom(i)).toSet
	val C = Domain("C", CSet)
	val hair = Relation("hair", C)
	val none = Relation("none", C)
	val has_covering = Relation("has_covering", A,C)
	val homeothermic = Relation("homeothermic", A)
	val scales = Relation("scales", C)
	val has_gills = Relation("has_gills", A)
	val feathers = Relation("feathers", C)
	val has_eggs = Relation("has_eggs", A)
	val has_milk = Relation("has_milk", A)
	val mammal = Relation("mammal", A)
	val fish = Relation("fish", A,A)
	val reptile = Relation("reptile", A)
	val bird = Relation("bird", A,A)
	val noneTuples = Set(3).map { case (x0) => DTuple(Atom(x0)) }
	val homeothermicTuples = Set(0,1,2,3,13,14,15,16).map { case (x0) => DTuple(Atom(x0)) }
	val scalesTuples = Set(1).map { case (x0) => DTuple(Atom(x0)) }
	val fishTuples = Set(4,5,6,7,(0, 5),(1, 5),(3, 11),(5, 0),(5, 11),(6, 0),(6, 4),(6, 13),(7, 9),(9, 5),(9, 7),(10, 7),(11, 0),(11, 11),(12, 10),(13, 14),(13, 18),(14, 11),(15, 20),(17, 17),(18, 8),(18, 11),(19, 14),(19, 16),(19, 19)).map { case (x0) => DTuple(Atom(x0)) }
	val feathersTuples = Set(0).map { case (x0) => DTuple(Atom(x0)) }
	val has_eggsTuples = Set(2,4,5,6,7,8,9,10,11,12,13,14,15).map { case (x0) => DTuple(Atom(x0)) }
	val hairTuples = Set(2).map { case (x0) => DTuple(Atom(x0)) }
	val has_coveringTuples = Set((0, 2),(1, 3),(2, 2),(3, 2),(4, 3),(5, 3),(6, 3),(7, 3),(8, 1),(9, 1),(10, 1),(11, 1),(12, 1),(13, 0),(14, 0),(15, 0)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val has_gillsTuples = Set(4,5,6,7).map { case (x0) => DTuple(Atom(x0)) }
	val mammalTuples = Set(0,1,2,3).map { case (x0) => DTuple(Atom(x0)) }
	val reptileTuples = Set(9,10,11,12).map { case (x0) => DTuple(Atom(x0)) }
	val birdTuples = Set(13,14,15,(1, 6),(1, 13),(2, 5),(3, 5),(6, 19),(7, 5),(8, 2),(10, 13),(11, 2),(11, 16),(11, 18),(12, 2),(13, 6),(14, 0),(15, 19),(16, 5),(17, 7),(17, 11),(18, 18),(19, 5),(19, 13)).map { case (x0) => DTuple(Atom(x0)) }
	val has_milkTuples = Set(0,1,2,3,16).map { case (x0) => DTuple(Atom(x0)) }
override val edb = Config(
hair -> (Instance[FValue](hair) ++ hairTuples.map(t => t -> FValue.One).toMap),
none -> (Instance[FValue](none) ++ noneTuples.map(t => t -> FValue.One).toMap),
has_covering -> (Instance[FValue](has_covering) ++ has_coveringTuples.map(t => t -> FValue.One).toMap),
homeothermic -> (Instance[FValue](homeothermic) ++ homeothermicTuples.map(t => t -> FValue.One).toMap),
scales -> (Instance[FValue](scales) ++ scalesTuples.map(t => t -> FValue.One).toMap),
has_gills -> (Instance[FValue](has_gills) ++ has_gillsTuples.map(t => t -> FValue.One).toMap),
feathers -> (Instance[FValue](feathers) ++ feathersTuples.map(t => t -> FValue.One).toMap),
has_eggs -> (Instance[FValue](has_eggs) ++ has_eggsTuples.map(t => t -> FValue.One).toMap),
has_milk -> (Instance[FValue](has_milk) ++ has_milkTuples.map(t => t -> FValue.One).toMap),
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
		Rule(4, FValue(0.5, Token(4)), bird(x0A),has_eggs(x0A)),
		Rule(5, FValue(0.5, Token(5)), bird(x0A),has_milk(x0A)),
		Rule(6, FValue(0.5, Token(6)), bird(x0A),has_gills(x0A)),
		Rule(7, FValue(0.5, Token(7)), bird(x0A),has_covering(x0A,x1C)),
		Rule(8, FValue(0.5, Token(8)), bird(x0A),has_covering(x0A,x1C),reptile(x0A)),
		Rule(9, FValue(0.5, Token(9)), bird(x0A),has_covering(x0A,x1C),mammal(x0A)),
		Rule(10, FValue(0.5, Token(10)), bird(x0A),fish(x0A),has_covering(x0A,x1C)),
		Rule(11, FValue(0.5, Token(11)), bird(x0A),has_covering(x0A,x1C),homeothermic(x0A)),
		Rule(12, FValue(0.5, Token(12)), bird(x0A),has_covering(x0A,x1C),has_eggs(x0A)),
		Rule(13, FValue(0.5, Token(13)), bird(x0A),has_covering(x0A,x1C),has_milk(x0A)),
		Rule(14, FValue(0.5, Token(14)), bird(x0A),has_covering(x0A,x1C),has_gills(x0A)),
		Rule(15, FValue(0.5, Token(15)), bird(x0A),feathers(x1C),has_covering(x0A,x1C)),
		Rule(16, FValue(0.5, Token(16)), bird(x0A),has_covering(x0A,x1C),scales(x1C)),
		Rule(17, FValue(0.5, Token(17)), bird(x0A),hair(x1C),has_covering(x0A,x1C)),
		Rule(18, FValue(0.5, Token(18)), bird(x0A),has_covering(x0A,x1C),none(x1C)),
		Rule(19, FValue(0.5, Token(19)), fish(x0A),reptile(x0A)),
		Rule(20, FValue(0.5, Token(20)), fish(x0A),mammal(x0A)),
		Rule(21, FValue(0.5, Token(21)), fish(x0A),bird(x0A)),
		Rule(22, FValue(0.5, Token(22)), fish(x0A),homeothermic(x0A)),
		Rule(23, FValue(0.5, Token(23)), fish(x0A),has_eggs(x0A)),
		Rule(24, FValue(0.5, Token(24)), fish(x0A),has_milk(x0A)),
		Rule(25, FValue(0.5, Token(25)), fish(x0A),has_gills(x0A)),
		Rule(26, FValue(0.5, Token(26)), fish(x0A),has_covering(x0A,x1C)),
		Rule(27, FValue(0.5, Token(27)), fish(x0A),has_covering(x0A,x1C),reptile(x0A)),
		Rule(28, FValue(0.5, Token(28)), fish(x0A),has_covering(x0A,x1C),mammal(x0A)),
		Rule(29, FValue(0.5, Token(29)), fish(x0A),bird(x0A),has_covering(x0A,x1C)),
		Rule(30, FValue(0.5, Token(30)), fish(x0A),has_covering(x0A,x1C),homeothermic(x0A)),
		Rule(31, FValue(0.5, Token(31)), fish(x0A),has_covering(x0A,x1C),has_eggs(x0A)),
		Rule(32, FValue(0.5, Token(32)), fish(x0A),has_covering(x0A,x1C),has_milk(x0A)),
		Rule(33, FValue(0.5, Token(33)), fish(x0A),has_covering(x0A,x1C),has_gills(x0A)),
		Rule(34, FValue(0.5, Token(34)), fish(x0A),feathers(x1C),has_covering(x0A,x1C)),
		Rule(35, FValue(0.5, Token(35)), fish(x0A),has_covering(x0A,x1C),scales(x1C)),
		Rule(36, FValue(0.5, Token(36)), fish(x0A),hair(x1C),has_covering(x0A,x1C)),
		Rule(37, FValue(0.5, Token(37)), fish(x0A),has_covering(x0A,x1C),none(x1C)),
		Rule(38, FValue(0.5, Token(38)), mammal(x0A),reptile(x0A)),
		Rule(39, FValue(0.5, Token(39)), mammal(x0A),fish(x0A)),
		Rule(40, FValue(0.5, Token(40)), mammal(x0A),bird(x0A)),
		Rule(41, FValue(0.5, Token(41)), mammal(x0A),homeothermic(x0A)),
		Rule(42, FValue(0.5, Token(42)), mammal(x0A),has_eggs(x0A)),
		Rule(43, FValue(0.5, Token(43)), mammal(x0A),has_milk(x0A)),
		Rule(44, FValue(0.5, Token(44)), mammal(x0A),has_gills(x0A)),
		Rule(45, FValue(0.5, Token(45)), mammal(x0A),has_covering(x0A,x1C)),
		Rule(46, FValue(0.5, Token(46)), mammal(x0A),has_covering(x0A,x1C),reptile(x0A)),
		Rule(47, FValue(0.5, Token(47)), mammal(x0A),fish(x0A),has_covering(x0A,x1C)),
		Rule(48, FValue(0.5, Token(48)), mammal(x0A),bird(x0A),has_covering(x0A,x1C)),
		Rule(49, FValue(0.5, Token(49)), mammal(x0A),has_covering(x0A,x1C),homeothermic(x0A)),
		Rule(50, FValue(0.5, Token(50)), mammal(x0A),has_covering(x0A,x1C),has_eggs(x0A)),
		Rule(51, FValue(0.5, Token(51)), mammal(x0A),has_covering(x0A,x1C),has_milk(x0A)),
		Rule(52, FValue(0.5, Token(52)), mammal(x0A),has_covering(x0A,x1C),has_gills(x0A)),
		Rule(53, FValue(0.5, Token(53)), mammal(x0A),feathers(x1C),has_covering(x0A,x1C)),
		Rule(54, FValue(0.5, Token(54)), mammal(x0A),has_covering(x0A,x1C),scales(x1C)),
		Rule(55, FValue(0.5, Token(55)), mammal(x0A),hair(x1C),has_covering(x0A,x1C)),
		Rule(56, FValue(0.5, Token(56)), mammal(x0A),has_covering(x0A,x1C),none(x1C)),
		Rule(57, FValue(0.5, Token(57)), reptile(x0A),mammal(x0A)),
		Rule(58, FValue(0.5, Token(58)), reptile(x0A),fish(x0A)),
		Rule(59, FValue(0.5, Token(59)), reptile(x0A),bird(x0A)),
		Rule(60, FValue(0.5, Token(60)), reptile(x0A),homeothermic(x0A)),
		Rule(61, FValue(0.5, Token(61)), reptile(x0A),has_eggs(x0A)),
		Rule(62, FValue(0.5, Token(62)), reptile(x0A),has_milk(x0A)),
		Rule(63, FValue(0.5, Token(63)), reptile(x0A),has_gills(x0A)),
		Rule(64, FValue(0.5, Token(64)), reptile(x0A),has_covering(x0A,x1C)),
		Rule(65, FValue(0.5, Token(65)), reptile(x0A),has_covering(x0A,x1C),mammal(x0A)),
		Rule(66, FValue(0.5, Token(66)), reptile(x0A),fish(x0A),has_covering(x0A,x1C)),
		Rule(67, FValue(0.5, Token(67)), reptile(x0A),bird(x0A),has_covering(x0A,x1C)),
		Rule(68, FValue(0.5, Token(68)), reptile(x0A),has_covering(x0A,x1C),homeothermic(x0A)),
		Rule(69, FValue(0.5, Token(69)), reptile(x0A),has_covering(x0A,x1C),has_eggs(x0A)),
		Rule(70, FValue(0.5, Token(70)), reptile(x0A),has_covering(x0A,x1C),has_milk(x0A)),
		Rule(71, FValue(0.5, Token(71)), reptile(x0A),has_covering(x0A,x1C),has_gills(x0A)),
		Rule(72, FValue(0.5, Token(72)), reptile(x0A),feathers(x1C),has_covering(x0A,x1C)),
		Rule(73, FValue(0.5, Token(73)), reptile(x0A),has_covering(x0A,x1C),scales(x1C)),
		Rule(74, FValue(0.5, Token(74)), reptile(x0A),hair(x1C),has_covering(x0A,x1C)),
		Rule(75, FValue(0.5, Token(75)), reptile(x0A),has_covering(x0A,x1C),none(x1C)),
	)
	val soupProg = Program("animalsSoup", soup)
	val evaluator = SeminaiveEvaluator(soupProg)

	override val expected = Set(15,25,51,73)
	override val maxVarCount: Int = 20
}
