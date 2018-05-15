package qd
package learner

import org.scalatest.{FunSuite, Ignore}

@Ignore
class CallSite2 extends FunSuite {
  val CSet : Set[Atom] = Range(0, 5).map(i => Atom(i)).toSet
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
  val invocation: Relation = Relation("invocation", C, C, H, C, C, M)
  val actual: Relation = Relation("actual", H, Z, V)
  val formal: Relation = Relation("formal", M, Z, V)
  val assign: Relation = Relation("assign", C, C, V, C, C, V)

  val points_initialTuples: Set[DTuple] = Set((1,1),(2,2),(5,1),(9,9),(11,11)).map{ case (a,b) => DTuple(Atom(a), Atom(b)) }
  val storeTuples : Set[DTuple] = Set((7,2,5),(5,1,1),(9,2,5),(9,1,11)).map{ case (a,b,c) => DTuple(Atom(a), Atom(b), Atom(c)) }
  val loadTuples : Set[DTuple] = Set((2,2,8),(5,1,10),(5,1,7)).map{ case (a,b,c) => DTuple(Atom(a), Atom(b), Atom(c)) }
  val invocationTuples : Set[DTuple] = Set((1,1,1,2,3,1),(3,1,2,2,3,1),(2,3,9,1,1,1)).map{ case (a,b,c,d,e,f) =>
    DTuple(Atom(a), Atom(b), Atom(c), Atom(d), Atom(e), Atom(f)) }
  val actualTuples : Set[DTuple] = Set((1,0,5),(1,1,7),(2,0,6),(2,1,2)).map{ case (a,b,c) => DTuple(Atom(a), Atom(b), Atom(c)) }
  val formalTuples : Set[DTuple]  = Set((1,0,5),(1,1,7),(2,0,3),(2,1,6)).map{ case (a,b,c) => DTuple(Atom(a), Atom(b), Atom(c)) }
  val assignTuples : Set[DTuple] =
    Set((2,3,5,1,1,5),(2,3,7,1,1,7),(2,3,5,3,1,6),(2,3,7,3,1,2),(2,3,8,3,4,2),(2,3,8,2,4,2),(2,3,8,2,1,2)).map{
    case (a,b,c,d,e,f) => DTuple(Atom(a), Atom(b), Atom(c), Atom(d), Atom(e), Atom(f))
  }

  val pointsto: Relation = Relation("pointsto", C, C, V, H)
  val heappointsto: Relation = Relation("heappointsto", H, F, H)

  val pointstoTuples : Set[DTuple] = Set((1,1,1,1),(1,1,5,1),(3,1,2,2),(2,3,9,9),(2,3,5,1),
    (2,3,7,2),(1,1,10,1),(1,1,7,1),(2,3,7,1),(3,1,8,1),(2,3,10,1)).map{ case (a,b,c,d) => DTuple(Atom(a), Atom(b), Atom(c), Atom(d)) }
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
  val x8H: Variable = Variable("x8H", H)

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
  val x5M: Variable = Variable("x5M", M)
  val x6M: Variable = Variable("x6M", M)
  val x7M: Variable = Variable("x7M", M)

  // expected: 1, 8, 61, 93
  val soup : Set[Rule] = Set(
    Rule(1	,Value(0.5, Token(1	)),pointsto(x2C,x3C,x0V,x1H), invocation(x2C,x3C,x1H,x4C,x5C,x6M),points_initial(x0V,x1H)),
    Rule(2	,Value(0.5, Token(2	)),pointsto(x2C,x4C,x0V,x1H), invocation(x2C,x3C,x1H,x4C,x5C,x6M),points_initial(x0V,x1H)),
    Rule(3	,Value(0.5, Token(3	)),pointsto(x2C,x5C,x0V,x1H), invocation(x2C,x3C,x1H,x4C,x5C,x6M),points_initial(x0V,x1H)),
    Rule(4	,Value(0.5, Token(4	)),pointsto(x3C,x2C,x0V,x1H), invocation(x2C,x3C,x1H,x4C,x5C,x6M),points_initial(x0V,x1H)),
    Rule(5	,Value(0.5, Token(5	)),pointsto(x3C,x4C,x0V,x1H), invocation(x2C,x3C,x1H,x4C,x5C,x6M),points_initial(x0V,x1H)),
    Rule(6	,Value(0.5, Token(6	)),pointsto(x3C,x5C,x0V,x1H), invocation(x2C,x3C,x1H,x4C,x5C,x6M),points_initial(x0V,x1H)),
    Rule(7	,Value(0.5, Token(7	)),pointsto(x4C,x2C,x0V,x1H), invocation(x2C,x3C,x1H,x4C,x5C,x6M),points_initial(x0V,x1H)),
    Rule(8	,Value(0.5, Token(8	)),pointsto(x4C,x3C,x0V,x1H), invocation(x2C,x3C,x1H,x4C,x5C,x6M),points_initial(x0V,x1H)),
    Rule(9	,Value(0.5, Token(9	)),pointsto(x5C,x2C,x0V,x1H), invocation(x2C,x3C,x1H,x4C,x5C,x6M),points_initial(x0V,x1H)),
    Rule(10	,Value(0.5, Token(10	)),pointsto(x5C,x3C,x0V,x1H), invocation(x2C,x3C,x1H,x4C,x5C,x6M),points_initial(x0V,x1H)),
    Rule(11	,Value(0.5, Token(11	)),pointsto(x2C,x3C,x0V,x1H), assign(x2C,x3C,x0V,x4C,x5C,x6V),points_initial(x0V,x1H)),
    Rule(12	,Value(0.5, Token(12	)),pointsto(x2C,x3C,x0V,x1H), assign(x2C,x3C,x4V,x5C,x6C,x0V),points_initial(x0V,x1H)),
    Rule(13	,Value(0.5, Token(13	)),pointsto(x2C,x3C,x4V,x1H), assign(x2C,x3C,x4V,x5C,x6C,x0V),points_initial(x0V,x1H)),
    Rule(14	,Value(0.5, Token(14	)),pointsto(x2C,x5C,x4V,x1H), assign(x2C,x3C,x4V,x5C,x6C,x0V),points_initial(x0V,x1H)),
    Rule(15	,Value(0.5, Token(15	)),pointsto(x2C,x6C,x4V,x1H), assign(x2C,x3C,x4V,x5C,x6C,x0V),points_initial(x0V,x1H)),
    Rule(16	,Value(0.5, Token(16	)),pointsto(x5C,x3C,x4V,x1H), assign(x2C,x3C,x4V,x5C,x6C,x0V),points_initial(x0V,x1H)),
    Rule(17	,Value(0.5, Token(17	)),pointsto(x6C,x3C,x4V,x1H), assign(x2C,x3C,x4V,x5C,x6C,x0V),points_initial(x0V,x1H)),
    Rule(18	,Value(0.5, Token(18	)),pointsto(x2C,x3C,x0V,x1H), points_initial(x0V,x1H),pointsto(x2C,x3C,x0V,x4H)),
    Rule(19	,Value(0.5, Token(19	)),pointsto(x3C,x2C,x0V,x1H), points_initial(x0V,x1H),pointsto(x2C,x3C,x0V,x4H)),
    Rule(20	,Value(0.5, Token(20	)),pointsto(x3C,x2C,x0V,x4H), points_initial(x0V,x1H),pointsto(x2C,x3C,x0V,x4H)),
    Rule(21	,Value(0.5, Token(21	)),pointsto(x2C,x3C,x0V,x1H), points_initial(x0V,x1H),pointsto(x2C,x3C,x4V,x1H)),
    Rule(22	,Value(0.5, Token(22	)),pointsto(x3C,x2C,x0V,x1H), points_initial(x0V,x1H),pointsto(x2C,x3C,x4V,x1H)),
    Rule(23	,Value(0.5, Token(23	)),pointsto(x3C,x2C,x4V,x1H), points_initial(x0V,x1H),pointsto(x2C,x3C,x4V,x1H)),
    Rule(24	,Value(0.5, Token(24	)),pointsto(x3C,x4C,x5V,x0H), actual(x0H,x1Z,x2V),assign(x3C,x4C,x5V,x6C,x7C,x2V)),
    Rule(25	,Value(0.5, Token(25	)),pointsto(x3C,x4C,x2V,x0H), actual(x0H,x1Z,x2V),pointsto(x3C,x4C,x5V,x0H)),
    Rule(26	,Value(0.5, Token(26	)),pointsto(x3C,x4C,x2V,x0H), actual(x0H,x1Z,x2V),pointsto(x3C,x4C,x2V,x5H)),
    Rule(27	,Value(0.5, Token(27	)),pointsto(x3C,x4C,x2V,x5H), pointsto(x3C,x4C,x0V,x5H),store(x0V,x1F,x2V)),
    Rule(28	,Value(0.5, Token(28	)),pointsto(x3C,x4C,x2V,x5H), load(x0V,x1F,x2V),pointsto(x3C,x4C,x0V,x5H)),
    Rule(29	,Value(0.5, Token(29	)),pointsto(x4C,x3C,x0V,x5H), pointsto(x3C,x4C,x0V,x5H),store(x0V,x1F,x2V)),
    Rule(30	,Value(0.5, Token(30	)),pointsto(x4C,x3C,x0V,x5H), load(x0V,x1F,x2V),pointsto(x3C,x4C,x0V,x5H)),
    Rule(31	,Value(0.5, Token(31	)),pointsto(x0C,x1C,x7V,x8H), invocation(x0C,x1C,x2H,x3C,x4C,x5M),pointsto(x3C,x6C,x7V,x8H)),
    Rule(32	,Value(0.5, Token(32	)),pointsto(x0C,x1C,x7V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x3C,x6C,x7V,x8H)),
    Rule(33	,Value(0.5, Token(33	)),pointsto(x0C,x1C,x7V,x8H), invocation(x0C,x1C,x2H,x3C,x4C,x5M),pointsto(x6C,x4C,x7V,x8H)),
    Rule(34	,Value(0.5, Token(34	)),pointsto(x0C,x1C,x7V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x4C,x7V,x8H)),
    Rule(35	,Value(0.5, Token(35	)),pointsto(x0C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x0C,x6C,x7V,x8H)),
    Rule(36	,Value(0.5, Token(36	)),pointsto(x0C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x0C,x7V,x8H)),
    Rule(37	,Value(0.5, Token(37	)),pointsto(x0C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x1C,x6C,x7V,x8H)),
    Rule(38	,Value(0.5, Token(38	)),pointsto(x0C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x1C,x7V,x8H)),
    Rule(39	,Value(0.5, Token(39	)),pointsto(x0C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x7C,x2V,x8H)),
    Rule(40	,Value(0.5, Token(40	)),pointsto(x0C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x3C,x6C,x7V,x8H)),
    Rule(41	,Value(0.5, Token(41	)),pointsto(x0C,x1C,x5V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x3C,x6C,x7V,x8H)),
    Rule(42	,Value(0.5, Token(42	)),pointsto(x0C,x3C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x3C,x6C,x7V,x8H)),
    Rule(43	,Value(0.5, Token(43	)),pointsto(x0C,x4C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x3C,x6C,x7V,x8H)),
    Rule(44	,Value(0.5, Token(44	)),pointsto(x0C,x6C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x3C,x6C,x7V,x8H)),
    Rule(45	,Value(0.5, Token(45	)),pointsto(x3C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x3C,x6C,x7V,x8H)),
    Rule(46	,Value(0.5, Token(46	)),pointsto(x4C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x3C,x6C,x7V,x8H)),
    Rule(47	,Value(0.5, Token(47	)),pointsto(x6C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x3C,x6C,x7V,x8H)),
    Rule(48	,Value(0.5, Token(48	)),pointsto(x0C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x3C,x7V,x8H)),
    Rule(49	,Value(0.5, Token(49	)),pointsto(x0C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x4C,x6C,x7V,x8H)),
    Rule(50	,Value(0.5, Token(50	)),pointsto(x0C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x4C,x7V,x8H)),
    Rule(51	,Value(0.5, Token(51	)),pointsto(x0C,x1C,x5V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x4C,x7V,x8H)),
    Rule(52	,Value(0.5, Token(52	)),pointsto(x0C,x3C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x4C,x7V,x8H)),
    Rule(53	,Value(0.5, Token(53	)),pointsto(x0C,x4C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x4C,x7V,x8H)),
    Rule(54	,Value(0.5, Token(54	)),pointsto(x0C,x6C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x4C,x7V,x8H)),
    Rule(55	,Value(0.5, Token(55	)),pointsto(x3C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x4C,x7V,x8H)),
    Rule(56	,Value(0.5, Token(56	)),pointsto(x4C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x4C,x7V,x8H)),
    Rule(57	,Value(0.5, Token(57	)),pointsto(x6C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x4C,x7V,x8H)),
    Rule(58	,Value(0.5, Token(58	)),pointsto(x0C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x7C,x5V,x8H)),
    Rule(59	,Value(0.5, Token(59	)),pointsto(x0C,x1C,x5V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x7C,x5V,x8H)),
    Rule(60	,Value(0.5, Token(60	)),pointsto(x0C,x3C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x7C,x5V,x8H)),
    Rule(61	,Value(0.5, Token(61	)),pointsto(x0C,x4C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x7C,x5V,x8H)),
    Rule(62	,Value(0.5, Token(62	)),pointsto(x0C,x6C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x7C,x5V,x8H)),
    Rule(63	,Value(0.5, Token(63	)),pointsto(x0C,x7C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x7C,x5V,x8H)),
    Rule(64	,Value(0.5, Token(64	)),pointsto(x3C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x7C,x5V,x8H)),
    Rule(65	,Value(0.5, Token(65	)),pointsto(x4C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x7C,x5V,x8H)),
    Rule(66	,Value(0.5, Token(66	)),pointsto(x6C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x7C,x5V,x8H)),
    Rule(67	,Value(0.5, Token(67	)),pointsto(x7C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x7C,x5V,x8H)),
    Rule(68	,Value(0.5, Token(68	)),pointsto(x3C,x4C,x5V,x2H), heappointsto(x0H,x1F,x2H),pointsto(x3C,x4C,x5V,x0H)),
    Rule(69	,Value(0.5, Token(69	)),pointsto(x4C,x3C,x5V,x2H), heappointsto(x0H,x1F,x2H),pointsto(x3C,x4C,x5V,x0H)),
    Rule(70	,Value(0.5, Token(70	)),pointsto(x0C,x1C,x2V,x6H), pointsto(x0C,x1C,x2V,x3H),pointsto(x0C,x4C,x5V,x6H)),
    Rule(71	,Value(0.5, Token(71	)),pointsto(x0C,x1C,x2V,x6H), pointsto(x0C,x1C,x2V,x3H),pointsto(x4C,x0C,x5V,x6H)),
    Rule(72	,Value(0.5, Token(72	)),pointsto(x4C,x0C,x5V,x3H), pointsto(x0C,x1C,x2V,x3H),pointsto(x4C,x0C,x5V,x6H)),
    Rule(73	,Value(0.5, Token(73	)),pointsto(x0C,x1C,x2V,x6H), pointsto(x0C,x1C,x2V,x3H),pointsto(x4C,x1C,x5V,x6H)),
    Rule(74	,Value(0.5, Token(74	)),pointsto(x0C,x1C,x2V,x6H), pointsto(x0C,x1C,x2V,x3H),pointsto(x4C,x5C,x2V,x6H)),
    Rule(75	,Value(0.5, Token(75	)),pointsto(x0C,x1C,x2V,x3H), assign(x0C,x1C,x2V,x4C,x5C,x6V),pointsto(x4C,x5C,x6V,x3H)),
    Rule(76	,Value(0.5, Token(76	)),pointsto(x0C,x1C,x2V,x3H), heappointsto(x6H,x5F,x3H),pointsto(x0C,x1C,x4V,x6H),store(x4V,x5F,x2V)),
    Rule(77	,Value(0.5, Token(77	)),pointsto(x0C,x1C,x2V,x3H), heappointsto(x6H,x5F,x3H),load(x4V,x5F,x2V),pointsto(x0C,x1C,x4V,x6H)),
    Rule(78	,Value(0.5, Token(78	)),heappointsto(x3H,x2F,x1H), heappointsto(x1H,x2F,x3H),points_initial(x0V,x1H)),
    Rule(79	,Value(0.5, Token(79	)),heappointsto(x4H,x3F,x0H), actual(x0H,x1Z,x2V),heappointsto(x0H,x3F,x4H)),
    Rule(80	,Value(0.5, Token(80	)),heappointsto(x0H,x4F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(81	,Value(0.5, Token(81	)),heappointsto(x0H,x4F,x3H), actual(x0H,x1Z,x2V),heappointsto(x3H,x4F,x0H)),
    Rule(82	,Value(0.5, Token(82	)),heappointsto(x0H,x1F,x4H), heappointsto(x0H,x1F,x2H),heappointsto(x0H,x3F,x4H)),
    Rule(83	,Value(0.5, Token(83	)),heappointsto(x2H,x1F,x4H), heappointsto(x0H,x1F,x2H),heappointsto(x0H,x3F,x4H)),
    Rule(84	,Value(0.5, Token(84	)),heappointsto(x2H,x3F,x4H), heappointsto(x0H,x1F,x2H),heappointsto(x0H,x3F,x4H)),
    Rule(85	,Value(0.5, Token(85	)),heappointsto(x0H,x1F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(86	,Value(0.5, Token(86	)),heappointsto(x0H,x4F,x2H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(87	,Value(0.5, Token(87	)),heappointsto(x2H,x1F,x0H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(88	,Value(0.5, Token(88	)),heappointsto(x2H,x1F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(89	,Value(0.5, Token(89	)),heappointsto(x2H,x4F,x0H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(90	,Value(0.5, Token(90	)),heappointsto(x2H,x4F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(91	,Value(0.5, Token(91	)),heappointsto(x3H,x1F,x0H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(92	,Value(0.5, Token(92	)),heappointsto(x3H,x1F,x2H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(93	,Value(0.5, Token(93	)),heappointsto(x3H,x4F,x2H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(94	,Value(0.5, Token(94	)),heappointsto(x0H,x1F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x1F,x4H)),
    Rule(95	,Value(0.5, Token(95	)),heappointsto(x0H,x1F,x4H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x1F,x4H)),
    Rule(96	,Value(0.5, Token(96	)),heappointsto(x2H,x1F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x1F,x4H)),
    Rule(97	,Value(0.5, Token(97	)),heappointsto(x2H,x1F,x4H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x1F,x4H)),
    Rule(98	,Value(0.5, Token(98	)),heappointsto(x0H,x1F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x2H)),
    Rule(99	,Value(0.5, Token(99	)),heappointsto(x0H,x4F,x2H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x2H)),
    Rule(100	,Value(0.5, Token(100	)),heappointsto(x0H,x4F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x2H)),
    Rule(101	,Value(0.5, Token(101	)),heappointsto(x0H,x1F,x2H), pointsto(x5C,x6C,x3V,x0H),pointsto(x5C,x6C,x4V,x2H),store(x3V,x1F,x4V)),
    Rule(102	,Value(0.5, Token(102	)),heappointsto(x0H,x1F,x2H), load(x3V,x1F,x4V),pointsto(x5C,x6C,x3V,x0H),pointsto(x5C,x6C,x4V,x2H)),
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