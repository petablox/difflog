package qd
package learner

import org.scalatest.{FunSuite, Ignore}

@Ignore
class CallSite1 extends FunSuite {
  val CSet : Set[Atom] = Range(0, 4).map(i => Atom(i)).toSet
  val C : Domain = Domain("C", CSet)

  val VSet : Set[Atom] = Range(0, 12).map(i => Atom(i)).toSet
  val V : Domain = Domain("V", VSet)

  val HSet : Set[Atom] = Range(0, 12).map(i => Atom(i)).toSet
  val H : Domain = Domain("H", HSet)

  val MSet : Set[Atom] = Range(0, 3).map(i => Atom(i)).toSet
  val M : Domain = Domain("M", MSet)

  val ZSet : Set[Atom] = Range(0, 3).map(i => Atom(i)).toSet
  val Z : Domain = Domain("Z", ZSet)

  val FSet : Set[Atom] = Range(0, 3).map(i => Atom(i)).toSet
  val F : Domain = Domain("F", FSet)

  val points_initial: Relation = Relation("points_initial", V, H)
  val store: Relation = Relation("store", V, F, V)
  val load: Relation = Relation("load", V, F, V)
  val invocation: Relation = Relation("invocation", C, H, C, M)
  val actual: Relation = Relation("actual", H, Z, V)
  val formal: Relation = Relation("formal", M, Z, V)
  val assign: Relation = Relation("assign", C, V, C, V)

  val points_initialTuples: Set[DTuple] = Set((1,1),(2,2),(5,1),(9,9),(11,11)).map{ case (a,b) => DTuple(Atom(a), Atom(b)) }
  val storeTuples : Set[DTuple] = Set((7,2,5),(5,1,1),(9,2,5),(9,1,11)).map{ case (a,b,c) => DTuple(Atom(a), Atom(b), Atom(c)) }
  val loadTuples : Set[DTuple] = Set((2,2,8),(5,1,10),(5,1,7)).map{ case (a,b,c) => DTuple(Atom(a), Atom(b), Atom(c)) }
  val invocationTuples : Set[DTuple] = Set((1,1,2,1),(3,2,2,1),(2,9,1,1),(3,11,1,1)).map{ case (a,b,c,d) =>
    DTuple(Atom(a), Atom(b), Atom(c), Atom(d)) }
  val actualTuples : Set[DTuple] = Set((1,0,5),(1,1,7),(2,0,6),(2,1,2)).map{ case (a,b,c) => DTuple(Atom(a), Atom(b), Atom(c)) }
  val formalTuples : Set[DTuple]  = Set((1,0,5),(1,1,7),(2,0,3),(2,1,6)).map{ case (a,b,c) => DTuple(Atom(a), Atom(b), Atom(c)) }
  val assignTuples : Set[DTuple] = Set((2,5,1,5),(2,7,1,7),(2,5,3,6),(2,7,3,2)).map{ case (a,b,c,d) => DTuple(Atom(a), Atom(b), Atom(c), Atom(d)) }

  val pointsto: Relation = Relation("pointsto", C, V, H)
  val heappointsto: Relation = Relation("heappointsto", H, F, H)

  val pointstoTuples : Set[DTuple] = Set((1,1,1),(1,5,1),(3,2,2),(3,11,11),(2,9,9),(2,5,1),
    (2,7,2),(1,10,1),(1,7,1),(2,7,1),(3,8,1),(2,10,1)).map{ case (a,b,c) => DTuple(Atom(a), Atom(b), Atom(c)) }
  val heappointstoTuples : Set[DTuple] = Set((1,1,1),(2,2,1),(1,2,1),(9,2,1)).map{ case (a,b,c) => DTuple(Atom(a), Atom(b), Atom(c)) }

  val edb : Config = Config(
    points_initial -> (Instance(points_initial) ++ points_initialTuples.map(t => t -> One).toMap),
    store -> (Instance(store) ++ storeTuples.map(t => t -> One).toMap),
    load -> (Instance(load) ++ loadTuples.map(t => t -> One).toMap),
    invocation -> (Instance(invocation) ++ invocationTuples.map(t => t -> One).toMap),
    actual -> (Instance(actual) ++ actualTuples.map(t => t -> One).toMap),
    formal -> (Instance(formal) ++ formalTuples.map(t => t -> One).toMap),
    assign -> (Instance(assign) ++ assignTuples.map(t => t -> One).toMap),
  )

  val refOut : Config = Config (
    pointsto -> (Instance(pointsto) ++ pointstoTuples.map(t => t -> One).toMap),
    heappointsto -> (Instance(heappointsto) ++ heappointstoTuples.map(t => t -> One).toMap)
  )

  val x0H: Variable = Variable("x0H", H)
  val x1H: Variable = Variable("x1H", H)
  val x2H: Variable = Variable("x2H", H)
  val x3H: Variable = Variable("x3H", H)
  val x4H: Variable = Variable("x4H", H)
  val x5H: Variable = Variable("x5H", H)
  val x6H: Variable = Variable("x6H", H)
  val x7H: Variable = Variable("x7H", H)

  val x0C: Variable = Variable("x0C", C)
  val x1C: Variable = Variable("x1C", C)
  val x2C: Variable = Variable("x2C", C)
  val x3C: Variable = Variable("x3C", C)
  val x4C: Variable = Variable("x4C", C)
  val x5C: Variable = Variable("x5C", C)
  val x6C: Variable = Variable("x6C", C)
  val x7C: Variable = Variable("x7C", C)

  val x1F: Variable = Variable("x1F", F)
  val x2F: Variable = Variable("x2F", F)
  val x3F: Variable = Variable("x3F", F)
  val x4F: Variable = Variable("x4F", F)
  val x5F: Variable = Variable("x5F", F)
  val x6F: Variable = Variable("x6F", F)

  val x0V: Variable = Variable("x0V", V)
  val x1V: Variable = Variable("x1V", V)
  val x2V: Variable = Variable("x2V", V)
  val x3V: Variable = Variable("x3V", V)
  val x4V: Variable = Variable("x4V", V)
  val x5V: Variable = Variable("x5V", V)
  val x6V: Variable = Variable("x6V", V)
  val x7V: Variable = Variable("x7V", V)

  val x1Z: Variable = Variable("x1Z", Z)
  val x2Z: Variable = Variable("x2Z", Z)
  val x3Z: Variable = Variable("x3Z", Z)
  val x4Z: Variable = Variable("x4Z", Z)

  val x0M: Variable = Variable("x0M", M)
  val x1M: Variable = Variable("x1M", M)
  val x2M: Variable = Variable("x2M", M)
  val x3M: Variable = Variable("x3M", M)
  val x4M: Variable = Variable("x4M", M)

  val soup : Set[Rule] = Set(
    Rule(1,Value(0.5, Token(1)),pointsto(x2C,x0V,x1H), invocation(x2C,x1H,x3C,x4M),points_initial(x0V,x1H)),
    Rule(2,Value(0.5, Token(2)),pointsto(x3C,x0V,x1H), invocation(x2C,x1H,x3C,x4M),points_initial(x0V,x1H)),
    Rule(3,Value(0.5, Token(3)),pointsto(x2C,x0V,x1H), assign(x2C,x0V,x3C,x4V),points_initial(x0V,x1H)),
    Rule(4,Value(0.5, Token(4)),pointsto(x2C,x4V,x1H), assign(x2C,x0V,x3C,x4V),points_initial(x0V,x1H)),
    Rule(5,Value(0.5, Token(5)),pointsto(x3C,x0V,x1H), assign(x2C,x0V,x3C,x4V),points_initial(x0V,x1H)),
    Rule(6,Value(0.5, Token(6)),pointsto(x3C,x4V,x1H), assign(x2C,x0V,x3C,x4V),points_initial(x0V,x1H)),
    Rule(7,Value(0.5, Token(7)),pointsto(x2C,x0V,x1H), assign(x2C,x3V,x4C,x0V),points_initial(x0V,x1H)),
    Rule(8,Value(0.5, Token(8)),pointsto(x2C,x3V,x1H), assign(x2C,x3V,x4C,x0V),points_initial(x0V,x1H)),
    Rule(9,Value(0.5, Token(9)),pointsto(x4C,x0V,x1H), assign(x2C,x3V,x4C,x0V),points_initial(x0V,x1H)),
    Rule(10,Value(0.5, Token(10)),pointsto(x4C,x3V,x1H), assign(x2C,x3V,x4C,x0V),points_initial(x0V,x1H)),
    Rule(11,Value(0.5, Token(11)),pointsto(x2C,x0V,x1H), points_initial(x0V,x1H),pointsto(x2C,x0V,x3H)),
    Rule(12,Value(0.5, Token(12)),pointsto(x2C,x0V,x1H), points_initial(x0V,x1H),pointsto(x2C,x3V,x1H)),
    Rule(13,Value(0.5, Token(13)),pointsto(x3C,x2V,x0H), actual(x0H,x1Z,x2V),assign(x3C,x2V,x4C,x5V)),
    Rule(14,Value(0.5, Token(14)),pointsto(x3C,x4V,x0H), actual(x0H,x1Z,x2V),assign(x3C,x4V,x5C,x2V)),
    Rule(15,Value(0.5, Token(15)),pointsto(x3C,x2V,x0H), actual(x0H,x1Z,x2V),pointsto(x3C,x4V,x0H)),
    Rule(16,Value(0.5, Token(16)),pointsto(x3C,x2V,x0H), actual(x0H,x1Z,x2V),pointsto(x3C,x2V,x4H)),
    Rule(17,Value(0.5, Token(17)),pointsto(x3C,x2V,x4H), pointsto(x3C,x0V,x4H),store(x0V,x1F,x2V)),
    Rule(18,Value(0.5, Token(18)),pointsto(x3C,x2V,x4H), load(x0V,x1F,x2V),pointsto(x3C,x0V,x4H)),
    Rule(19,Value(0.5, Token(19)),pointsto(x3C,x0V,x4H), pointsto(x3C,x2V,x4H),store(x0V,x1F,x2V)),
    Rule(20,Value(0.5, Token(20)),pointsto(x3C,x0V,x4H), load(x0V,x1F,x2V),pointsto(x3C,x2V,x4H)),
    Rule(21,Value(0.5, Token(21)),pointsto(x4C,x5V,x1H), assign(x4C,x5V,x0C,x6V),invocation(x0C,x1H,x2C,x3M)),
    Rule(22,Value(0.5, Token(22)),pointsto(x0C,x5V,x1H), invocation(x0C,x1H,x2C,x3M),pointsto(x4C,x5V,x1H)),
    Rule(23,Value(0.5, Token(23)),pointsto(x0C,x4V,x1H), invocation(x0C,x1H,x2C,x3M),pointsto(x2C,x4V,x5H)),
    Rule(24,Value(0.5, Token(24)),pointsto(x0C,x4V,x5H), invocation(x0C,x1H,x2C,x3M),pointsto(x2C,x4V,x5H)),
    Rule(25,Value(0.5, Token(25)),pointsto(x0C,x4V,x5H), assign(x0C,x1V,x2C,x3V),pointsto(x2C,x4V,x5H)),
    Rule(26,Value(0.5, Token(26)),pointsto(x0C,x1V,x5H), assign(x0C,x1V,x2C,x3V),pointsto(x0C,x4V,x5H)),
    Rule(27,Value(0.5, Token(27)),pointsto(x0C,x3V,x5H), assign(x0C,x1V,x2C,x3V),pointsto(x0C,x4V,x5H)),
    Rule(28,Value(0.5, Token(28)),pointsto(x2C,x1V,x5H), assign(x0C,x1V,x2C,x3V),pointsto(x0C,x4V,x5H)),
    Rule(29,Value(0.5, Token(29)),pointsto(x0C,x1V,x5H), assign(x0C,x1V,x2C,x3V),pointsto(x4C,x1V,x5H)),
    Rule(30,Value(0.5, Token(30)),pointsto(x0C,x3V,x5H), assign(x0C,x1V,x2C,x3V),pointsto(x4C,x1V,x5H)),
    Rule(31,Value(0.5, Token(31)),pointsto(x2C,x1V,x5H), assign(x0C,x1V,x2C,x3V),pointsto(x4C,x1V,x5H)),
    Rule(32,Value(0.5, Token(32)),pointsto(x0C,x1V,x5H), assign(x0C,x1V,x2C,x3V),pointsto(x2C,x4V,x5H)),
    Rule(33,Value(0.5, Token(33)),pointsto(x0C,x3V,x5H), assign(x0C,x1V,x2C,x3V),pointsto(x2C,x4V,x5H)),
    Rule(34,Value(0.5, Token(34)),pointsto(x2C,x1V,x5H), assign(x0C,x1V,x2C,x3V),pointsto(x2C,x4V,x5H)),
    Rule(35,Value(0.5, Token(35)),pointsto(x2C,x3V,x5H), assign(x0C,x1V,x2C,x3V),pointsto(x2C,x4V,x5H)),
    Rule(36,Value(0.5, Token(36)),pointsto(x0C,x1V,x5H), assign(x0C,x1V,x2C,x3V),pointsto(x4C,x3V,x5H)),
    Rule(37,Value(0.5, Token(37)),pointsto(x0C,x3V,x5H), assign(x0C,x1V,x2C,x3V),pointsto(x4C,x3V,x5H)),
    Rule(38,Value(0.5, Token(38)),pointsto(x2C,x1V,x5H), assign(x0C,x1V,x2C,x3V),pointsto(x4C,x3V,x5H)),
    Rule(39,Value(0.5, Token(39)),pointsto(x2C,x3V,x5H), assign(x0C,x1V,x2C,x3V),pointsto(x4C,x3V,x5H)),
    Rule(40,Value(0.5, Token(40)),pointsto(x4C,x1V,x5H), assign(x0C,x1V,x2C,x3V),pointsto(x4C,x3V,x5H)),
    Rule(41,Value(0.5, Token(41)),pointsto(x3C,x4V,x2H), heappointsto(x0H,x1F,x2H),pointsto(x3C,x4V,x0H)),
    Rule(42,Value(0.5, Token(42)),pointsto(x3C,x4V,x0H), heappointsto(x0H,x1F,x2H),pointsto(x3C,x4V,x2H)),
    Rule(43,Value(0.5, Token(43)),pointsto(x0C,x1V,x4H), pointsto(x0C,x1V,x2H),pointsto(x0C,x3V,x4H)),
    Rule(44,Value(0.5, Token(44)),pointsto(x0C,x1V,x4H), pointsto(x0C,x1V,x2H),pointsto(x3C,x1V,x4H)),
    Rule(45,Value(0.5, Token(45)),pointsto(x0C,x4V,x2H), pointsto(x0C,x1V,x2H),pointsto(x3C,x4V,x2H)),
    Rule(46,Value(0.5, Token(46)),pointsto(x2C,x5V,x1H), assign(x2C,x5V,x0C,x6V),invocation(x0C,x1H,x2C,x3M)),
    Rule(47,Value(0.5, Token(47)),pointsto(x4C,x5V,x1H), assign(x4C,x5V,x0C,x6V),invocation(x0C,x1H,x0C,x3M)),
    Rule(48,Value(0.5, Token(48)),pointsto(x2C,x4V,x5H), assign(x0C,x4V,x2C,x3V),pointsto(x0C,x4V,x5H)),
    Rule(49,Value(0.5, Token(49)),pointsto(x2C,x4V,x5H), assign(x0C,x1V,x2C,x4V),pointsto(x0C,x4V,x5H)),
    Rule(50,Value(0.5, Token(50)),pointsto(x0C,x5V,x1H), invocation(x0C,x1H,x4C,x3M),pointsto(x4C,x5V,x1H)),
    Rule(51,Value(0.5, Token(51)),pointsto(x0C,x4V,x5H), assign(x0C,x4V,x2C,x3V),pointsto(x2C,x4V,x5H)),
    Rule(52,Value(0.5, Token(52)),pointsto(x0C,x4V,x5H), assign(x0C,x1V,x2C,x4V),pointsto(x2C,x4V,x5H)),
    Rule(53,Value(0.5, Token(53)),pointsto(x0C,x1V,x5H), assign(x0C,x1V,x2C,x4V),pointsto(x0C,x4V,x5H)),
    Rule(54,Value(0.5, Token(54)),pointsto(x0C,x3V,x5H), assign(x0C,x4V,x2C,x3V),pointsto(x0C,x4V,x5H)),
    Rule(55,Value(0.5, Token(55)),pointsto(x2C,x1V,x5H), assign(x0C,x1V,x2C,x4V),pointsto(x0C,x4V,x5H)),
    Rule(56,Value(0.5, Token(56)),pointsto(x0C,x3V,x5H), assign(x0C,x1V,x4C,x3V),pointsto(x4C,x1V,x5H)),
    Rule(57,Value(0.5, Token(57)),pointsto(x2C,x3V,x5H), assign(x0C,x1V,x2C,x3V),pointsto(x2C,x1V,x5H)),
    Rule(58,Value(0.5, Token(58)),pointsto(x0C,x1V,x5H), assign(x0C,x1V,x2C,x4V),pointsto(x2C,x4V,x5H)),
    Rule(59,Value(0.5, Token(59)),pointsto(x2C,x1V,x5H), assign(x0C,x1V,x2C,x4V),pointsto(x2C,x4V,x5H)),
    Rule(60,Value(0.5, Token(60)),pointsto(x0C,x1V,x2H), heappointsto(x5H,x4F,x2H),pointsto(x0C,x3V,x5H),store(x3V,x4F,x1V)),
    Rule(61,Value(0.5, Token(61)),pointsto(x0C,x1V,x2H), heappointsto(x5H,x4F,x2H),load(x3V,x4F,x1V),pointsto(x0C,x3V,x5H)),
    Rule(62,Value(0.5, Token(62)),heappointsto(x2H,x1F,x0H), heappointsto(x0H,x1F,x2H)),
    Rule(63,Value(0.5, Token(63)),heappointsto(x3H,x2F,x1H), heappointsto(x1H,x2F,x3H),points_initial(x0V,x1H)),
    Rule(64,Value(0.5, Token(64)),heappointsto(x1H,x3F,x2H), heappointsto(x2H,x3F,x1H),points_initial(x0V,x1H)),
    Rule(65,Value(0.5, Token(65)),heappointsto(x0H,x4F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(66,Value(0.5, Token(66)),heappointsto(x0H,x4F,x3H), actual(x0H,x1Z,x2V),heappointsto(x3H,x4F,x0H)),
    Rule(67,Value(0.5, Token(67)),heappointsto(x4H,x1F,x3H), heappointsto(x3H,x1F,x4H),store(x0V,x1F,x2V)),
    Rule(68,Value(0.5, Token(68)),heappointsto(x4H,x1F,x3H), heappointsto(x3H,x1F,x4H),load(x0V,x1F,x2V)),
    Rule(69,Value(0.5, Token(69)),heappointsto(x0H,x1F,x4H), heappointsto(x0H,x1F,x2H),heappointsto(x0H,x3F,x4H)),
    Rule(70,Value(0.5, Token(70)),heappointsto(x2H,x1F,x4H), heappointsto(x0H,x1F,x2H),heappointsto(x0H,x3F,x4H)),
    Rule(71,Value(0.5, Token(71)),heappointsto(x2H,x3F,x4H), heappointsto(x0H,x1F,x2H),heappointsto(x0H,x3F,x4H)),
    Rule(72,Value(0.5, Token(72)),heappointsto(x0H,x1F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(73,Value(0.5, Token(73)),heappointsto(x0H,x4F,x2H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(74,Value(0.5, Token(74)),heappointsto(x2H,x1F,x0H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(75,Value(0.5, Token(75)),heappointsto(x2H,x1F,x0H), heappointsto(x0H,x1F,x2H),pointsto(x3C,x4V,x0H)),
    Rule(76,Value(0.5, Token(76)),heappointsto(x2H,x4F,x0H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(77,Value(0.5, Token(77)),heappointsto(x2H,x4F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(78,Value(0.5, Token(78)),heappointsto(x3H,x1F,x0H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(79,Value(0.5, Token(79)),heappointsto(x3H,x1F,x2H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(80,Value(0.5, Token(80)),heappointsto(x3H,x4F,x2H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(81,Value(0.5, Token(81)),heappointsto(x0H,x1F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x1F,x4H)),
    Rule(82,Value(0.5, Token(82)),heappointsto(x0H,x1F,x4H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x1F,x4H)),
    Rule(83,Value(0.5, Token(83)),heappointsto(x2H,x1F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x1F,x4H)),
    Rule(84,Value(0.5, Token(84)),heappointsto(x2H,x1F,x4H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x1F,x4H)),
    Rule(85,Value(0.5, Token(85)),heappointsto(x0H,x1F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x2H)),
    Rule(86,Value(0.5, Token(86)),heappointsto(x0H,x4F,x2H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x2H)),
    Rule(87,Value(0.5, Token(87)),heappointsto(x0H,x4F,x3H), heappointsto(x0H,x4F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(88,Value(0.5, Token(88)),heappointsto(x0H,x4F,x3H), heappointsto(x0H,x1F,x3H),heappointsto(x3H,x4F,x0H)),
    Rule(89,Value(0.5, Token(89)),heappointsto(x4H,x1F,x3H), heappointsto(x0H,x1F,x3H),heappointsto(x3H,x1F,x4H)),
    Rule(90,Value(0.5, Token(90)),heappointsto(x2H,x1F,x4H), heappointsto(x0H,x1F,x2H),heappointsto(x0H,x1F,x4H)),
    Rule(91,Value(0.5, Token(91)),heappointsto(x2H,x1F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x1F,x0H)),
    Rule(92,Value(0.5, Token(92)),heappointsto(x3H,x1F,x2H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x1F,x0H)),
    Rule(93,Value(0.5, Token(93)),heappointsto(x0H,x1F,x2H), pointsto(x5C,x3V,x0H),pointsto(x5C,x4V,x2H),store(x3V,x1F,x4V)),
    Rule(94,Value(0.5, Token(94)),heappointsto(x0H,x1F,x2H), load(x3V,x1F,x4V),pointsto(x5C,x3V,x0H),pointsto(x5C,x4V,x2H))
  )

  val soupProg: Program = Program("1CallSiteSoup", soup)
  val evaluator = SeminaiveEvaluator(soupProg)

  test(s"Applying evaluator ${evaluator.name} to program ${soupProg.name}") {
    val startTime = System.nanoTime()
    val idb = evaluator(edb)
    val endTime = System.nanoTime()
    println(s"A ${idb(pointsto).support.size}. ${(endTime - startTime) / 1.0e9}")
  }
}