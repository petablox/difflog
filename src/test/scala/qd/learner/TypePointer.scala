package qd
package learner

class TypePointer extends Problem {
  override val name: String = "TypePointer"

  val h: Domain = Domain("H", Range(0, 12).map(i => Atom(i)).toSet)
  val f: Domain = Domain("F", Range(0, 3).map(i => Atom(i)).toSet)
  val v: Domain = Domain("V", Range(0, 12).map(i => Atom(i)).toSet)
  val z: Domain = Domain("Z", Range(0, 2).map(i => Atom(i)).toSet)
  val m: Domain = Domain("M", Range(0, 4).map(i => Atom(i)).toSet)
  val t: Domain = Domain("T", Range(0, 4).map(i => Atom(i)).toSet)

  val points_initial: Relation = Relation("points_initial", v, h)
  val load: Relation = Relation("load", v, f, v)
  val store: Relation = Relation("store", v, f, v)
  val assign: Relation = Relation("assign", t, v, t, v)
  val invocation: Relation = Relation("invocation", h, m)
  val actual: Relation = Relation("actual", h, z, v)
  val formal: Relation = Relation("formal", m, z, v)
  val receiver_actual: Relation = Relation("receiver_actual", m, v)
  val receiver_formal: Relation = Relation("receiver_formal", h, v)
  val pointsto: Relation = Relation("pointsto", t, v, h)
  val heappointsto: Relation = Relation("heappointsto", h, f, h)
  val enclosing_type : Relation = Relation("enclosing_type", v, t)

  val pointsInitialTuples: Set[DTuple] = Set((1,1),(2,2),(5,1),(9,3),(11,11)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  val loadTuples: Set[DTuple] = Set((9,1,1),(5,2,8),(1,2,4),(5,2,1),(9,1,4)).map { case (a, b, c) => DTuple(Atom(a), Atom(b), Atom(c)) }
  val storeTuples: Set[DTuple] = Set((5,1,1),(5,2,11),(1,2,5),(4,1,2),(5,1,2)).map { case (a, b, c) => DTuple(Atom(a), Atom(b), Atom(c)) }
  val assignTuples: Set[DTuple] = Set((2,2,1,1)).map { case (a, b, c, d) => DTuple(Atom(a), Atom(b), Atom(c), Atom(d)) }
  val invocationTuples: Set[DTuple] = Set((1,1),(2,2),(5,1),(9,3),(11,1)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  val actualTuples: Set[DTuple] = Set((1,0,8),(2,0,7),(5,0,4),(9,0,2),(11,0,1)).map { case (a, b, c) => DTuple(Atom(a), Atom(b), Atom(c)) }
  val formalTuples: Set[DTuple] = Set((1,0,2),(2,0,3),(3,0,4)).map { case (a, b, c) => DTuple(Atom(a), Atom(b), Atom(c)) }
  val receiverActualTuples: Set[DTuple] = Set((1,3),(2,4),(3,5)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  val receiverFormalTuples: Set[DTuple] = Set((1,2),(2,10),(5,3),(9,4),(11,6)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  val enclosingTypeTuples : Set[DTuple] = Set((1,1),(2,2),(5,1),(9,3),(11,1)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }

  override val edb: Config = Config(
    points_initial -> (Instance(points_initial) ++ pointsInitialTuples.map(t => t -> One).toMap),
    load -> (Instance(load) ++ loadTuples.map(t => t -> One).toMap),
    store -> (Instance(store) ++ storeTuples.map(t => t -> One).toMap),
    assign -> (Instance(assign) ++ assignTuples.map(t => t -> One).toMap),
    invocation -> (Instance(invocation) ++ invocationTuples.map(t => t -> One).toMap),
    actual -> (Instance(actual) ++ actualTuples.map(t => t -> One).toMap),
    formal -> (Instance(formal) ++ formalTuples.map(t => t -> One).toMap),
    receiver_actual -> (Instance(receiver_actual) ++ receiverActualTuples.map(t => t -> One).toMap),
    receiver_formal -> (Instance(receiver_formal) ++ receiverFormalTuples.map(t => t -> One).toMap),
    enclosing_type -> (Instance(enclosing_type) ++ enclosingTypeTuples.map(t => t -> One).toMap),
  )

  val pointstoTuples: Set[DTuple] = Set((1,1,1),(2,2,2),(1,5,5),(3,9,9),(1,11,11),
    (2,2,1),(1,4,5),(1,8,11),(1,1,11),(2,2,11)).map { case (a, b, c) => DTuple(Atom(a), Atom(b), Atom(c)) }
  val heappointstoTuples: Set[DTuple] = Set((5,2,11),(5,1,1),(5,1,11),(11,2,5),(1,2,5)).map { case (a, b, c) => DTuple(Atom(a), Atom(b), Atom(c)) }
  override val refOut: Config = Config(pointsto -> (Instance(pointsto) ++ pointstoTuples.map(t => t -> One).toMap),
    heappointsto -> (Instance(heappointsto) ++ heappointstoTuples.map(t => t -> One).toMap))

  val x0H: Variable = Variable("x0H", h)
  val x1H: Variable = Variable("x1H", h)
  val x2H: Variable = Variable("x2H", h)
  val x3H: Variable = Variable("x3H", h)
  val x4H: Variable = Variable("x4H", h)
  val x5H: Variable = Variable("x5H", h)
  val x6H: Variable = Variable("x6H", h)
  val x7H: Variable = Variable("x7H", h)

  val x1F: Variable = Variable("x1F", f)
  val x2F: Variable = Variable("x2F", f)
  val x3F: Variable = Variable("x3F", f)
  val x4F: Variable = Variable("x4F", f)
  val x5F: Variable = Variable("x5F", f)
  val x6F: Variable = Variable("x6F", f)

  val x0V: Variable = Variable("x0V", v)
  val x1V: Variable = Variable("x1V", v)
  val x2V: Variable = Variable("x2V", v)
  val x3V: Variable = Variable("x3V", v)
  val x4V: Variable = Variable("x4V", v)
  val x5V: Variable = Variable("x5V", v)
  val x6V: Variable = Variable("x6V", v)
  val x7V: Variable = Variable("x7V", v)

  val x0T: Variable = Variable("x0T", t)
  val x1T: Variable = Variable("x1T", t)
  val x2T: Variable = Variable("x2T", t)
  val x3T: Variable = Variable("x3T", t)
  val x4T: Variable = Variable("x4T", t)
  val x5T: Variable = Variable("x5T", t)
  val x6T: Variable = Variable("x6T", t)
  val x7T: Variable = Variable("x7T", t)

  val x1Z: Variable = Variable("x1Z", z)
  val x2Z: Variable = Variable("x2Z", z)
  val x3Z: Variable = Variable("x3Z", z)
  val x4Z: Variable = Variable("x4Z", z)

  val x0M: Variable = Variable("x0M", m)
  val x1M: Variable = Variable("x1M", m)
  val x2M: Variable = Variable("x2M", m)
  val x3M: Variable = Variable("x3M", m)
  val x4M: Variable = Variable("x4M", m)

  // Expected: 1, 31, 32, 38
  override val soup : Set[Rule] = Set(
  Rule(1,Value(0.5, Token(1)),pointsto(x1T,x0V,x2H), enclosing_type(x0V,x1T),points_initial(x0V,x2H)),
  Rule(2,Value(0.5, Token(2)),pointsto(x1T,x0V,x3H), enclosing_type(x0V,x1T),pointsto(x1T,x2V,x3H)),
  Rule(3,Value(0.5, Token(3)),pointsto(x2T,x0V,x1H), assign(x2T,x0V,x3T,x4V),points_initial(x0V,x1H)),
  Rule(4,Value(0.5, Token(4)),pointsto(x2T,x4V,x1H), assign(x2T,x0V,x3T,x4V),points_initial(x0V,x1H)),
  Rule(5,Value(0.5, Token(5)),pointsto(x3T,x0V,x1H), assign(x2T,x0V,x3T,x4V),points_initial(x0V,x1H)),
  Rule(6,Value(0.5, Token(6)),pointsto(x2T,x0V,x1H), assign(x2T,x3V,x4T,x0V),points_initial(x0V,x1H)),
  Rule(7,Value(0.5, Token(7)),pointsto(x2T,x3V,x1H), assign(x2T,x3V,x4T,x0V),points_initial(x0V,x1H)),
  Rule(8,Value(0.5, Token(8)),pointsto(x4T,x3V,x1H), assign(x2T,x3V,x4T,x0V),points_initial(x0V,x1H)),
  Rule(9,Value(0.5, Token(9)),pointsto(x2T,x0V,x1H), points_initial(x0V,x1H),pointsto(x2T,x0V,x3H)),
  Rule(10,Value(0.5, Token(10)),pointsto(x2T,x0V,x1H), points_initial(x0V,x1H),pointsto(x2T,x3V,x1H)),
  Rule(11,Value(0.5, Token(11)),pointsto(x2T,x1V,x0H), assign(x2T,x1V,x3T,x4V),receiver_formal(x0H,x1V)),
  Rule(12,Value(0.5, Token(12)),pointsto(x2T,x4V,x0H), assign(x2T,x1V,x3T,x4V),receiver_formal(x0H,x1V)),
  Rule(13,Value(0.5, Token(13)),pointsto(x2T,x3V,x0H), assign(x2T,x3V,x4T,x1V),receiver_formal(x0H,x1V)),
  Rule(14,Value(0.5, Token(14)),pointsto(x2T,x1V,x0H), pointsto(x2T,x3V,x0H),receiver_formal(x0H,x1V)),
  Rule(15,Value(0.5, Token(15)),pointsto(x3T,x0V,x4H), pointsto(x3T,x2V,x4H),store(x0V,x1F,x2V)),
  Rule(16,Value(0.5, Token(16)),pointsto(x3T,x0V,x4H), load(x0V,x1F,x2V),pointsto(x3T,x2V,x4H)),
  Rule(17,Value(0.5, Token(17)),pointsto(x0T,x1V,x5H), assign(x0T,x1V,x2T,x3V),pointsto(x0T,x4V,x5H)),
  Rule(18,Value(0.5, Token(18)),pointsto(x0T,x1V,x5H), assign(x0T,x1V,x2T,x3V),pointsto(x4T,x1V,x5H)),
  Rule(19,Value(0.5, Token(19)),pointsto(x0T,x1V,x5H), assign(x0T,x1V,x2T,x3V),pointsto(x2T,x4V,x5H)),
  Rule(20,Value(0.5, Token(20)),pointsto(x0T,x3V,x5H), assign(x0T,x1V,x2T,x3V),pointsto(x2T,x4V,x5H)),
  Rule(21,Value(0.5, Token(21)),pointsto(x0T,x4V,x5H), assign(x0T,x1V,x2T,x3V),pointsto(x2T,x4V,x5H)),
  Rule(22,Value(0.5, Token(22)),pointsto(x2T,x1V,x5H), assign(x0T,x1V,x2T,x3V),pointsto(x2T,x4V,x5H)),
  Rule(23,Value(0.5, Token(23)),pointsto(x0T,x1V,x5H), assign(x0T,x1V,x2T,x3V),pointsto(x4T,x3V,x5H)),
  Rule(24,Value(0.5, Token(24)),pointsto(x0T,x3V,x5H), assign(x0T,x1V,x2T,x3V),pointsto(x4T,x3V,x5H)),
  Rule(25,Value(0.5, Token(25)),pointsto(x2T,x1V,x5H), assign(x0T,x1V,x2T,x3V),pointsto(x4T,x3V,x5H)),
  Rule(26,Value(0.5, Token(26)),pointsto(x4T,x1V,x5H), assign(x0T,x1V,x2T,x3V),pointsto(x4T,x3V,x5H)),
  Rule(27,Value(0.5, Token(27)),pointsto(x3T,x4V,x2H), heappointsto(x0H,x1F,x2H),pointsto(x3T,x4V,x0H)),
  Rule(28,Value(0.5, Token(28)),pointsto(x0T,x1V,x4H), pointsto(x0T,x1V,x2H),pointsto(x0T,x3V,x4H)),
  Rule(29,Value(0.5, Token(29)),pointsto(x0T,x1V,x4H), pointsto(x0T,x1V,x2H),pointsto(x3T,x1V,x4H)),
  Rule(30,Value(0.5, Token(30)),pointsto(x0T,x1V,x2H), heappointsto(x5H,x4F,x2H),pointsto(x0T,x3V,x5H),store(x3V,x4F,x1V)),
  Rule(31,Value(0.5, Token(31)),pointsto(x0T,x1V,x2H), heappointsto(x5H,x4F,x2H),load(x3V,x4F,x1V),pointsto(x0T,x3V,x5H)),
  Rule(32,Value(0.5, Token(32)),pointsto(x0T,x1V,x2H), assign(x0T,x1V,x3T,x4V),pointsto(x3T,x4V,x2H)),
  Rule(33,Value(0.5, Token(33)),heappointsto(x0H,x1F,x4H), heappointsto(x0H,x1F,x2H),heappointsto(x0H,x3F,x4H)),
  Rule(34,Value(0.5, Token(34)),heappointsto(x0H,x4F,x2H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
  Rule(35,Value(0.5, Token(35)),heappointsto(x3H,x1F,x2H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
  Rule(36,Value(0.5, Token(36)),heappointsto(x3H,x4F,x2H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x4F,x0H)),
  Rule(37,Value(0.5, Token(37)),heappointsto(x0H,x1F,x4H), heappointsto(x0H,x1F,x2H),heappointsto(x3H,x1F,x4H)),
  Rule(38,Value(0.5, Token(38)),heappointsto(x0H,x1F,x2H), pointsto(x5T,x3V,x0H),pointsto(x5T,x4V,x2H),store(x3V,x1F,x4V)),
  Rule(39,Value(0.5, Token(39)),heappointsto(x0H,x1F,x2H), load(x3V,x1F,x4V),pointsto(x5T,x3V,x0H),pointsto(x5T,x4V,x2H)),
  )

  override val expected: Set[Any] = Set(1, 31, 32, 38)
  override val maxVarCount: Int = 20
}