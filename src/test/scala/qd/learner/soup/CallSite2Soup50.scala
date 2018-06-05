package qd
package learner

class CallSite2Soup extends Problem {
  override val name: String = "CallSite2"

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

  override val edb : Config[FValue] = Config(
    points_initial -> (Instance[FValue](points_initial) ++ points_initialTuples.map(t => t -> FValue.One).toMap),
    store -> (Instance[FValue](store) ++ storeTuples.map(t => t -> FValue.One).toMap),
    load -> (Instance[FValue](load) ++ loadTuples.map(t => t -> FValue.One).toMap),
    invocation -> (Instance[FValue](invocation) ++ invocationTuples.map(t => t -> FValue.One).toMap),
    actual -> (Instance[FValue](actual) ++ actualTuples.map(t => t -> FValue.One).toMap),
    formal -> (Instance[FValue](formal) ++ formalTuples.map(t => t -> FValue.One).toMap),
    assign -> (Instance[FValue](assign) ++ assignTuples.map(t => t -> FValue.One).toMap),
  )

  override val refOut : Config[FValue] = Config (
    pointsto -> (Instance[FValue](pointsto) ++ pointstoTuples.map(t => t -> FValue.One).toMap),
    heappointsto -> (Instance[FValue](heappointsto) ++ heappointstoTuples.map(t => t -> FValue.One).toMap)
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
  override val soup : Set[Rule[FValue]] = Set(
    Rule(1	,FValue(0.5, Token(1	)),pointsto(x2C,x3C,x0V,x1H), invocation(x2C,x3C,x1H,x4C,x5C,x6M),points_initial(x0V,x1H)),
    Rule(2	,FValue(0.5, Token(2	)),pointsto(x2C,x4C,x0V,x1H), invocation(x2C,x3C,x1H,x4C,x5C,x6M),points_initial(x0V,x1H)),
    Rule(3	,FValue(0.5, Token(3	)),pointsto(x2C,x5C,x0V,x1H), invocation(x2C,x3C,x1H,x4C,x5C,x6M),points_initial(x0V,x1H)),
    Rule(4	,FValue(0.5, Token(4	)),pointsto(x3C,x2C,x0V,x1H), invocation(x2C,x3C,x1H,x4C,x5C,x6M),points_initial(x0V,x1H)),
    Rule(5	,FValue(0.5, Token(5	)),pointsto(x3C,x4C,x0V,x1H), invocation(x2C,x3C,x1H,x4C,x5C,x6M),points_initial(x0V,x1H)),
    Rule(8	,FValue(0.5, Token(8	)),pointsto(x4C,x3C,x0V,x1H), invocation(x2C,x3C,x1H,x4C,x5C,x6M),points_initial(x0V,x1H)),
    Rule(9	,FValue(0.5, Token(9	)),pointsto(x5C,x2C,x0V,x1H), invocation(x2C,x3C,x1H,x4C,x5C,x6M),points_initial(x0V,x1H)),
    Rule(10	,FValue(0.5, Token(10	)),pointsto(x5C,x3C,x0V,x1H), invocation(x2C,x3C,x1H,x4C,x5C,x6M),points_initial(x0V,x1H)),
    Rule(11	,FValue(0.5, Token(11	)),pointsto(x2C,x3C,x0V,x1H), assign(x2C,x3C,x0V,x4C,x5C,x6V),points_initial(x0V,x1H)),
    Rule(12	,FValue(0.5, Token(12	)),pointsto(x2C,x3C,x0V,x1H), assign(x2C,x3C,x4V,x5C,x6C,x0V),points_initial(x0V,x1H)),
    Rule(13	,FValue(0.5, Token(13	)),pointsto(x2C,x3C,x4V,x1H), assign(x2C,x3C,x4V,x5C,x6C,x0V),points_initial(x0V,x1H)),
    Rule(14	,FValue(0.5, Token(14	)),pointsto(x2C,x5C,x4V,x1H), assign(x2C,x3C,x4V,x5C,x6C,x0V),points_initial(x0V,x1H)),
    Rule(15	,FValue(0.5, Token(15	)),pointsto(x2C,x6C,x4V,x1H), assign(x2C,x3C,x4V,x5C,x6C,x0V),points_initial(x0V,x1H)),
    Rule(16	,FValue(0.5, Token(16	)),pointsto(x5C,x3C,x4V,x1H), assign(x2C,x3C,x4V,x5C,x6C,x0V),points_initial(x0V,x1H)),
    Rule(22	,FValue(0.5, Token(22	)),pointsto(x3C,x2C,x0V,x1H), points_initial(x0V,x1H),pointsto(x2C,x3C,x4V,x1H)),
    Rule(23	,FValue(0.5, Token(23	)),pointsto(x3C,x2C,x4V,x1H), points_initial(x0V,x1H),pointsto(x2C,x3C,x4V,x1H)),
    Rule(24	,FValue(0.5, Token(24	)),pointsto(x3C,x4C,x5V,x0H), actual(x0H,x1Z,x2V),assign(x3C,x4C,x5V,x6C,x7C,x2V)),
    Rule(25	,FValue(0.5, Token(25	)),pointsto(x3C,x4C,x2V,x0H), actual(x0H,x1Z,x2V),pointsto(x3C,x4C,x5V,x0H)),
    Rule(26	,FValue(0.5, Token(26	)),pointsto(x3C,x4C,x2V,x0H), actual(x0H,x1Z,x2V),pointsto(x3C,x4C,x2V,x5H)),
    Rule(30	,FValue(0.5, Token(30	)),pointsto(x4C,x3C,x0V,x5H), load(x0V,x1F,x2V),pointsto(x3C,x4C,x0V,x5H)),
    Rule(32	,FValue(0.5, Token(32	)),pointsto(x0C,x1C,x7V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x3C,x6C,x7V,x8H)),
    Rule(34	,FValue(0.5, Token(34	)),pointsto(x0C,x1C,x7V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x4C,x7V,x8H)),
    Rule(36	,FValue(0.5, Token(36	)),pointsto(x0C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x0C,x7V,x8H)),
    Rule(37	,FValue(0.5, Token(37	)),pointsto(x0C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x1C,x6C,x7V,x8H)),
    Rule(38	,FValue(0.5, Token(38	)),pointsto(x0C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x1C,x7V,x8H)),
    Rule(43	,FValue(0.5, Token(43	)),pointsto(x0C,x4C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x3C,x6C,x7V,x8H)),
    Rule(44	,FValue(0.5, Token(44	)),pointsto(x0C,x6C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x3C,x6C,x7V,x8H)),
    Rule(45	,FValue(0.5, Token(45	)),pointsto(x3C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x3C,x6C,x7V,x8H)),
    Rule(46	,FValue(0.5, Token(46	)),pointsto(x4C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x3C,x6C,x7V,x8H)),
    Rule(57	,FValue(0.5, Token(57	)),pointsto(x6C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x4C,x7V,x8H)),
    Rule(58	,FValue(0.5, Token(58	)),pointsto(x0C,x1C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x7C,x5V,x8H)),
    Rule(59	,FValue(0.5, Token(59	)),pointsto(x0C,x1C,x5V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x7C,x5V,x8H)),
    Rule(61	,FValue(0.5, Token(61	)),pointsto(x0C,x4C,x2V,x8H), assign(x0C,x1C,x2V,x3C,x4C,x5V),pointsto(x6C,x7C,x5V,x8H)),
    Rule(70	,FValue(0.5, Token(70	)),pointsto(x0C,x1C,x2V,x6H), pointsto(x0C,x1C,x2V,x3H),pointsto(x0C,x4C,x5V,x6H)),
    Rule(71	,FValue(0.5, Token(71	)),pointsto(x0C,x1C,x2V,x6H), pointsto(x0C,x1C,x2V,x3H),pointsto(x4C,x0C,x5V,x6H)),
    Rule(72	,FValue(0.5, Token(72	)),pointsto(x4C,x0C,x5V,x3H), pointsto(x0C,x1C,x2V,x3H),pointsto(x4C,x0C,x5V,x6H)),
    Rule(78	,FValue(0.5, Token(78	)),heappointsto(x3H,x2F,x1H), heappointsto(x1H,x2F,x3H),points_initial(x0V,x1H)),
    Rule(80	,FValue(0.5, Token(80	)),heappointsto(x0H,x4F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(81	,FValue(0.5, Token(81	)),heappointsto(x0H,x4F,x3H), actual(x0H,x1Z,x2V),heappointsto(x3H,x4F,x0H)),
    Rule(82	,FValue(0.5, Token(82	)),heappointsto(x0H,x1F,x4H), heappointsto(x0H,x1F,x2H),heappointsto(x0H,x3F,x4H)),
    Rule(85	,FValue(0.5, Token(85	)),heappointsto(x0H,x1F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(86	,FValue(0.5, Token(86	)),heappointsto(x0H,x4F,x2H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(87	,FValue(0.5, Token(87	)),heappointsto(x2H,x1F,x0H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(88	,FValue(0.5, Token(88	)),heappointsto(x2H,x1F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(89	,FValue(0.5, Token(89	)),heappointsto(x2H,x4F,x0H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(90	,FValue(0.5, Token(90	)),heappointsto(x2H,x4F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(91	,FValue(0.5, Token(91	)),heappointsto(x3H,x1F,x0H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(92	,FValue(0.5, Token(92	)),heappointsto(x3H,x1F,x2H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(93	,FValue(0.5, Token(93	)),heappointsto(x3H,x4F,x2H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(101	,FValue(0.5, Token(101	)),heappointsto(x0H,x1F,x2H), pointsto(x5C,x6C,x3V,x0H),pointsto(x5C,x6C,x4V,x2H),store(x3V,x1F,x4V)),
    Rule(102	,FValue(0.5, Token(102	)),heappointsto(x0H,x1F,x2H), load(x3V,x1F,x4V),pointsto(x5C,x6C,x3V,x0H),pointsto(x5C,x6C,x4V,x2H)),
  )

  override val expected: Set[Any] = Set(1, 8, 61, 93)
  override val maxVarCount: Int = 20
}
