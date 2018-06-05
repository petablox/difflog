package qd
package learner

class CallSite1Soup75 extends Problem {
  override val name: String = "CallSite1"

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

  val edb : Config[FValue] = Config(
    points_initial -> (Instance[FValue](points_initial) ++ points_initialTuples.map(t => t -> FValue.One).toMap),
    store -> (Instance[FValue](store) ++ storeTuples.map(t => t -> FValue.One).toMap),
    load -> (Instance[FValue](load) ++ loadTuples.map(t => t -> FValue.One).toMap),
    invocation -> (Instance[FValue](invocation) ++ invocationTuples.map(t => t -> FValue.One).toMap),
    actual -> (Instance[FValue](actual) ++ actualTuples.map(t => t -> FValue.One).toMap),
    formal -> (Instance[FValue](formal) ++ formalTuples.map(t => t -> FValue.One).toMap),
    assign -> (Instance[FValue](assign) ++ assignTuples.map(t => t -> FValue.One).toMap),
  )

  val refOut : Config[FValue] = Config (
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

  // expected: 1, 8, 61, 93
  val soup : Set[Rule[FValue]] = Set(
    Rule(1,FValue(0.5, Token(1)),pointsto(x2C,x0V,x1H), invocation(x2C,x1H,x3C,x4M),points_initial(x0V,x1H)),
    Rule(2,FValue(0.5, Token(2)),pointsto(x3C,x0V,x1H), invocation(x2C,x1H,x3C,x4M),points_initial(x0V,x1H)),
    Rule(3,FValue(0.5, Token(3)),pointsto(x2C,x0V,x1H), assign(x2C,x0V,x3C,x4V),points_initial(x0V,x1H)),
    Rule(4,FValue(0.5, Token(4)),pointsto(x2C,x4V,x1H), assign(x2C,x0V,x3C,x4V),points_initial(x0V,x1H)),
    Rule(5,FValue(0.5, Token(5)),pointsto(x3C,x0V,x1H), assign(x2C,x0V,x3C,x4V),points_initial(x0V,x1H)),
    Rule(7,FValue(0.5, Token(7)),pointsto(x2C,x0V,x1H), assign(x2C,x3V,x4C,x0V),points_initial(x0V,x1H)),
    Rule(8,FValue(0.5, Token(8)),pointsto(x2C,x3V,x1H), assign(x2C,x3V,x4C,x0V),points_initial(x0V,x1H)),
    Rule(9,FValue(0.5, Token(9)),pointsto(x4C,x0V,x1H), assign(x2C,x3V,x4C,x0V),points_initial(x0V,x1H)),
    Rule(11,FValue(0.5, Token(11)),pointsto(x2C,x0V,x1H), points_initial(x0V,x1H),pointsto(x2C,x0V,x3H)),
    Rule(12,FValue(0.5, Token(12)),pointsto(x2C,x0V,x1H), points_initial(x0V,x1H),pointsto(x2C,x3V,x1H)),
    Rule(14,FValue(0.5, Token(14)),pointsto(x3C,x4V,x0H), actual(x0H,x1Z,x2V),assign(x3C,x4V,x5C,x2V)),
    Rule(15,FValue(0.5, Token(15)),pointsto(x3C,x2V,x0H), actual(x0H,x1Z,x2V),pointsto(x3C,x4V,x0H)),
    Rule(16,FValue(0.5, Token(16)),pointsto(x3C,x2V,x0H), actual(x0H,x1Z,x2V),pointsto(x3C,x2V,x4H)),
    Rule(17,FValue(0.5, Token(17)),pointsto(x3C,x2V,x4H), pointsto(x3C,x0V,x4H),store(x0V,x1F,x2V)),
    Rule(18,FValue(0.5, Token(18)),pointsto(x3C,x2V,x4H), load(x0V,x1F,x2V),pointsto(x3C,x0V,x4H)),
    Rule(19,FValue(0.5, Token(19)),pointsto(x3C,x0V,x4H), pointsto(x3C,x2V,x4H),store(x0V,x1F,x2V)),
    Rule(21,FValue(0.5, Token(21)),pointsto(x4C,x5V,x1H), assign(x4C,x5V,x0C,x6V),invocation(x0C,x1H,x2C,x3M)),
    Rule(22,FValue(0.5, Token(22)),pointsto(x0C,x5V,x1H), invocation(x0C,x1H,x2C,x3M),pointsto(x4C,x5V,x1H)),
    Rule(23,FValue(0.5, Token(23)),pointsto(x0C,x4V,x1H), invocation(x0C,x1H,x2C,x3M),pointsto(x2C,x4V,x5H)),
    Rule(36,FValue(0.5, Token(36)),pointsto(x0C,x1V,x5H), assign(x0C,x1V,x2C,x3V),pointsto(x4C,x3V,x5H)),
    Rule(37,FValue(0.5, Token(37)),pointsto(x0C,x3V,x5H), assign(x0C,x1V,x2C,x3V),pointsto(x4C,x3V,x5H)),
    Rule(38,FValue(0.5, Token(38)),pointsto(x2C,x1V,x5H), assign(x0C,x1V,x2C,x3V),pointsto(x4C,x3V,x5H)),
    Rule(39,FValue(0.5, Token(39)),pointsto(x2C,x3V,x5H), assign(x0C,x1V,x2C,x3V),pointsto(x4C,x3V,x5H)),
    Rule(46,FValue(0.5, Token(46)),pointsto(x2C,x5V,x1H), assign(x2C,x5V,x0C,x6V),invocation(x0C,x1H,x2C,x3M)),
    Rule(47,FValue(0.5, Token(47)),pointsto(x4C,x5V,x1H), assign(x4C,x5V,x0C,x6V),invocation(x0C,x1H,x0C,x3M)),
    Rule(48,FValue(0.5, Token(48)),pointsto(x2C,x4V,x5H), assign(x0C,x4V,x2C,x3V),pointsto(x0C,x4V,x5H)),
    Rule(49,FValue(0.5, Token(49)),pointsto(x2C,x4V,x5H), assign(x0C,x1V,x2C,x4V),pointsto(x0C,x4V,x5H)),
    Rule(50,FValue(0.5, Token(50)),pointsto(x0C,x5V,x1H), invocation(x0C,x1H,x4C,x3M),pointsto(x4C,x5V,x1H)),
    Rule(51,FValue(0.5, Token(51)),pointsto(x0C,x4V,x5H), assign(x0C,x4V,x2C,x3V),pointsto(x2C,x4V,x5H)),
    Rule(52,FValue(0.5, Token(52)),pointsto(x0C,x4V,x5H), assign(x0C,x1V,x2C,x4V),pointsto(x2C,x4V,x5H)),
    Rule(53,FValue(0.5, Token(53)),pointsto(x0C,x1V,x5H), assign(x0C,x1V,x2C,x4V),pointsto(x0C,x4V,x5H)),
    Rule(54,FValue(0.5, Token(54)),pointsto(x0C,x3V,x5H), assign(x0C,x4V,x2C,x3V),pointsto(x0C,x4V,x5H)),
    Rule(55,FValue(0.5, Token(55)),pointsto(x2C,x1V,x5H), assign(x0C,x1V,x2C,x4V),pointsto(x0C,x4V,x5H)),
    Rule(56,FValue(0.5, Token(56)),pointsto(x0C,x3V,x5H), assign(x0C,x1V,x4C,x3V),pointsto(x4C,x1V,x5H)),
    Rule(57,FValue(0.5, Token(57)),pointsto(x2C,x3V,x5H), assign(x0C,x1V,x2C,x3V),pointsto(x2C,x1V,x5H)),
    Rule(58,FValue(0.5, Token(58)),pointsto(x0C,x1V,x5H), assign(x0C,x1V,x2C,x4V),pointsto(x2C,x4V,x5H)),
    Rule(59,FValue(0.5, Token(59)),pointsto(x2C,x1V,x5H), assign(x0C,x1V,x2C,x4V),pointsto(x2C,x4V,x5H)),
    Rule(60,FValue(0.5, Token(60)),pointsto(x0C,x1V,x2H), heappointsto(x5H,x4F,x2H),pointsto(x0C,x3V,x5H),store(x3V,x4F,x1V)),
    Rule(61,FValue(0.5, Token(61)),pointsto(x0C,x1V,x2H), heappointsto(x5H,x4F,x2H),load(x3V,x4F,x1V),pointsto(x0C,x3V,x5H)),
    Rule(62,FValue(0.5, Token(62)),heappointsto(x2H,x1F,x0H), heappointsto(x0H,x1F,x2H)),
    Rule(63,FValue(0.5, Token(63)),heappointsto(x3H,x2F,x1H), heappointsto(x1H,x2F,x3H),points_initial(x0V,x1H)),
    Rule(64,FValue(0.5, Token(64)),heappointsto(x1H,x3F,x2H), heappointsto(x2H,x3F,x1H),points_initial(x0V,x1H)),
    Rule(65,FValue(0.5, Token(65)),heappointsto(x0H,x4F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(66,FValue(0.5, Token(66)),heappointsto(x0H,x4F,x3H), actual(x0H,x1Z,x2V),heappointsto(x3H,x4F,x0H)),
    Rule(67,FValue(0.5, Token(67)),heappointsto(x4H,x1F,x3H), heappointsto(x3H,x1F,x4H),store(x0V,x1F,x2V)),
    Rule(68,FValue(0.5, Token(68)),heappointsto(x4H,x1F,x3H), heappointsto(x3H,x1F,x4H),load(x0V,x1F,x2V)),
    Rule(69,FValue(0.5, Token(69)),heappointsto(x0H,x1F,x4H), heappointsto(x0H,x1F,x2H),heappointsto(x0H,x3F,x4H)),
    Rule(70,FValue(0.5, Token(70)),heappointsto(x2H,x1F,x4H), heappointsto(x0H,x1F,x2H),heappointsto(x0H,x3F,x4H)),
    Rule(71,FValue(0.5, Token(71)),heappointsto(x2H,x3F,x4H), heappointsto(x0H,x1F,x2H),heappointsto(x0H,x3F,x4H)),
    Rule(72,FValue(0.5, Token(72)),heappointsto(x0H,x1F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(73,FValue(0.5, Token(73)),heappointsto(x0H,x4F,x2H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(74,FValue(0.5, Token(74)),heappointsto(x2H,x1F,x0H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(75,FValue(0.5, Token(75)),heappointsto(x2H,x1F,x0H), heappointsto(x0H,x1F,x2H),pointsto(x3C,x4V,x0H)),
    Rule(76,FValue(0.5, Token(76)),heappointsto(x2H,x4F,x0H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(77,FValue(0.5, Token(77)),heappointsto(x2H,x4F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(78,FValue(0.5, Token(78)),heappointsto(x3H,x1F,x0H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(79,FValue(0.5, Token(79)),heappointsto(x3H,x1F,x2H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(80,FValue(0.5, Token(80)),heappointsto(x3H,x4F,x2H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(81,FValue(0.5, Token(81)),heappointsto(x0H,x1F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x1F,x4H)),
    Rule(82,FValue(0.5, Token(82)),heappointsto(x0H,x1F,x4H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x1F,x4H)),
    Rule(83,FValue(0.5, Token(83)),heappointsto(x2H,x1F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x1F,x4H)),
    Rule(84,FValue(0.5, Token(84)),heappointsto(x2H,x1F,x4H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x1F,x4H)),
    Rule(85,FValue(0.5, Token(85)),heappointsto(x0H,x1F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x2H)),
    Rule(86,FValue(0.5, Token(86)),heappointsto(x0H,x4F,x2H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x2H)),
    Rule(87,FValue(0.5, Token(87)),heappointsto(x0H,x4F,x3H), heappointsto(x0H,x4F,x2H),heappointsto(x3H,x4F,x0H)),
    Rule(88,FValue(0.5, Token(88)),heappointsto(x0H,x4F,x3H), heappointsto(x0H,x1F,x3H),heappointsto(x3H,x4F,x0H)),
    Rule(90,FValue(0.5, Token(90)),heappointsto(x2H,x1F,x4H), heappointsto(x0H,x1F,x2H),heappointsto(x0H,x1F,x4H)),
    Rule(91,FValue(0.5, Token(91)),heappointsto(x2H,x1F,x3H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x1F,x0H)),
    Rule(92,FValue(0.5, Token(92)),heappointsto(x3H,x1F,x2H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x1F,x0H)),
    Rule(93,FValue(0.5, Token(93)),heappointsto(x0H,x1F,x2H), pointsto(x5C,x3V,x0H),pointsto(x5C,x4V,x2H),store(x3V,x1F,x4V)),
    Rule(94,FValue(0.5, Token(94)),heappointsto(x0H,x1F,x2H), load(x3V,x1F,x4V),pointsto(x5C,x3V,x0H),pointsto(x5C,x4V,x2H))
  )

  override val expected: Set[Any] = Set(1, 8, 61, 93)
  override val maxVarCount: Int = 6
}
