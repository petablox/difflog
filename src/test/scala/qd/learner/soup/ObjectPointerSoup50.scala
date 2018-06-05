package qd
package learner

import org.scalatest.Ignore

class ObjectPointerSoup50 extends Problem {
  override val name: String = "ObjectPointer"

  val h: Domain = Domain("H", Range(0, 12).map(i => Atom(i)).toSet)
  val f: Domain = Domain("F", Range(0, 3).map(i => Atom(i)).toSet)
  val v: Domain = Domain("V", Range(0, 12).map(i => Atom(i)).toSet)
  val z: Domain = Domain("F", Range(0, 2).map(i => Atom(i)).toSet)
  val m: Domain = Domain("F", Range(0, 4).map(i => Atom(i)).toSet)

  val points_initial: Relation = Relation("points_initial", v, h)
  val load: Relation = Relation("load", v, f, v)
  val store: Relation = Relation("store", v, f, v)
  val assign: Relation = Relation("assign", v, v, v, v)
  val invocation: Relation = Relation("invocation", h, m)
  val actual: Relation = Relation("actual", h, z, v)
  val formal: Relation = Relation("formal", m, z, v)
  val receiver_actual: Relation = Relation("receiver_actual", m, v)
  val receiver_formal: Relation = Relation("receiver_formal", h, v)
  val pointsto: Relation = Relation("pointsto", v, v, h)
  val heappointsto: Relation = Relation("heappointsto", h, f, h)

  val pointsInitialTuples: Set[DTuple] = Set((1,1),(2,2),(5,5),(9,9),(11,11)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  val loadTuples: Set[DTuple] = Set((9,1,1),(5,2,8),(1,2,4),(9,1,4)).map { case (a, b, c) => DTuple(Atom(a), Atom(b), Atom(c)) }
  val storeTuples: Set[DTuple] = Set((5,1,1),(5,2,11),(1,2,5),(4,1,2),(5,1,2)).map { case (a, b, c) => DTuple(Atom(a), Atom(b), Atom(c)) }
  val assignTuples: Set[DTuple] = Set((3,2,2,8),(3,2,3,4),(3,2,6,1),(4,3,10,7),(5,4,4,2)).map { case (a, b, c, d) => DTuple(Atom(a), Atom(b), Atom(c), Atom(d)) }
  val invocationTuples: Set[DTuple] = Set((1,1),(2,2),(5,1),(9,3),(11,1)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  val actualTuples: Set[DTuple] = Set((1,0,8),(2,0,7),(5,0,4),(9,0,2),(11,0,1)).map { case (a, b, c) => DTuple(Atom(a), Atom(b), Atom(c)) }
  val formalTuples: Set[DTuple] = Set((1,0,2),(2,0,3),(3,0,4)).map { case (a, b, c) => DTuple(Atom(a), Atom(b), Atom(c)) }
  val receiverActualTuples: Set[DTuple] = Set((1,3),(2,4),(3,5)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  val receiverFormalTuples: Set[DTuple] = Set((1,2),(2,10),(5,3),(9,4),(11,6)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }

  override val edb: Config[FValue] = Config(points_initial -> (Instance[FValue](points_initial) ++ pointsInitialTuples.map(t => t -> FValue.One).toMap),
                           load -> (Instance[FValue](load) ++ loadTuples.map(t => t -> FValue.One).toMap),
                           store -> (Instance[FValue](store) ++ storeTuples.map(t => t -> FValue.One).toMap),
                           assign -> (Instance[FValue](assign) ++ assignTuples.map(t => t -> FValue.One).toMap),
                           invocation -> (Instance[FValue](invocation) ++ invocationTuples.map(t => t -> FValue.One).toMap),
                           actual -> (Instance[FValue](actual) ++ actualTuples.map(t => t -> FValue.One).toMap),
                           formal -> (Instance[FValue](formal) ++ formalTuples.map(t => t -> FValue.One).toMap),
                           receiver_actual -> (Instance[FValue](receiver_actual) ++ receiverActualTuples.map(t => t -> FValue.One).toMap),
                           receiver_formal -> (Instance[FValue](receiver_formal) ++ receiverFormalTuples.map(t => t -> FValue.One).toMap))

  val pointstoTuples: Set[DTuple] = Set((3,1,1),(4,2,2),(3,5,5),(5,9,9),(3,11,11),(5,4,2),(3,8,11),(3,4,5),(3,2,5)).map { case (a, b, c) => DTuple(Atom(a), Atom(b), Atom(c)) }
  val heappointstoTuples: Set[DTuple] = Set((5,2,11),(5,1,1),(5,1,5),(1,2,5)).map { case (a, b, c) => DTuple(Atom(a), Atom(b), Atom(c)) }
  override val refOut: Config[FValue] = Config(pointsto -> (Instance[FValue](pointsto) ++ pointstoTuples.map(t => t -> FValue.One).toMap),
                              heappointsto -> (Instance[FValue](heappointsto) ++ heappointstoTuples.map(t => t -> FValue.One).toMap))

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

  val x1Z: Variable = Variable("x1Z", z)
  val x2Z: Variable = Variable("x2Z", z)
  val x3Z: Variable = Variable("x3Z", z)
  val x4Z: Variable = Variable("x4Z", z)

  val x0M: Variable = Variable("x0M", m)
  val x1M: Variable = Variable("x1M", m)
  val x2M: Variable = Variable("x2M", m)
  val x3M: Variable = Variable("x3M", m)
  val x4M: Variable = Variable("x4M", m)

  // Expected: 83, 90, 386, 387
  def soup1: Set[Rule[FValue]] = Set(
    Rule(1,FValue(0.5,Token(1)),heappointsto(x2H,x4F,x1H),heappointsto(x3H,x4F,x2H),points_initial(x0V,x1H),receiver_formal(x2H,x0V)),
    Rule(2,FValue(0.5,Token(2)),heappointsto(x3H,x4F,x1H),heappointsto(x3H,x4F,x2H),points_initial(x0V,x1H),receiver_formal(x2H,x0V)),
    Rule(3,FValue(0.5,Token(3)),heappointsto(x1H,x2F,x4H),points_initial(x0V,x1H),points_initial(x3V,x4H),store(x0V,x2F,x3V)),
    Rule(4,FValue(0.5,Token(4)),heappointsto(x1H,x2F,x4H),load(x0V,x2F,x3V),points_initial(x0V,x1H),points_initial(x3V,x4H)),
    Rule(38,FValue(0.5,Token(38)),heappointsto(x0H,x5F,x4H),actual(x0H,x1Z,x2V),load(x3V,x5F,x6V),pointsto(x2V,x3V,x4H)),
    Rule(40,FValue(0.5,Token(40)),heappointsto(x4H,x1F,x6H),heappointsto(x3H,x1F,x4H),heappointsto(x3H,x5F,x6H),load(x0V,x1F,x2V)),
    Rule(42,FValue(0.5,Token(42)),heappointsto(x6H,x1F,x4H),heappointsto(x3H,x1F,x4H),heappointsto(x3H,x5F,x6H),load(x0V,x1F,x2V)),
    Rule(43,FValue(0.5,Token(43)),heappointsto(x5H,x1F,x4H),heappointsto(x3H,x1F,x4H),heappointsto(x5H,x6F,x3H),store(x0V,x1F,x2V)),
    Rule(46,FValue(0.5,Token(46)),heappointsto(x4H,x1F,x6H),heappointsto(x4H,x5F,x6H),load(x0V,x1F,x2V),pointsto(x3V,x0V,x4H)),
    Rule(47,FValue(0.5,Token(47)),heappointsto(x6H,x1F,x4H),heappointsto(x4H,x5F,x6H),pointsto(x3V,x2V,x4H),store(x0V,x1F,x2V)),
    Rule(48,FValue(0.5,Token(48)),heappointsto(x6H,x1F,x4H),heappointsto(x4H,x5F,x6H),load(x0V,x1F,x2V),pointsto(x3V,x2V,x4H)),
    Rule(49,FValue(0.5,Token(49)),heappointsto(x4H,x1F,x6H),pointsto(x3V,x0V,x4H),pointsto(x3V,x5V,x6H),store(x0V,x1F,x2V)),
    Rule(50,FValue(0.5,Token(50)),heappointsto(x4H,x1F,x6H),load(x0V,x1F,x2V),pointsto(x3V,x0V,x4H),pointsto(x3V,x5V,x6H))
  )

  def soup2: Set[Rule[FValue]] = Set(
    Rule(51,FValue(0.5,Token(51)),heappointsto(x6H,x1F,x4H),pointsto(x3V,x0V,x4H),pointsto(x3V,x5V,x6H),store(x0V,x1F,x2V)),
    Rule(52,FValue(0.5,Token(52)),heappointsto(x6H,x1F,x4H),load(x0V,x1F,x2V),pointsto(x3V,x0V,x4H),pointsto(x3V,x5V,x6H)),
    Rule(54,FValue(0.5,Token(54)),heappointsto(x4H,x1F,x6H),load(x0V,x1F,x2V),pointsto(x3V,x0V,x4H),pointsto(x5V,x3V,x6H)),
    Rule(55,FValue(0.5,Token(55)),heappointsto(x4H,x1F,x6H),pointsto(x3V,x2V,x4H),pointsto(x3V,x5V,x6H),store(x0V,x1F,x2V)),
    Rule(56,FValue(0.5,Token(56)),heappointsto(x4H,x1F,x6H),load(x0V,x1F,x2V),pointsto(x3V,x2V,x4H),pointsto(x3V,x5V,x6H)),
    Rule(63,FValue(0.5,Token(63)),heappointsto(x2H,x1F,x5H),heappointsto(x0H,x1F,x2H),points_initial(x3V,x2H),pointsto(x3V,x4V,x5H)),
    Rule(83,FValue(0.5,Token(83)),heappointsto(x0H,x1F,x2H),pointsto(x5V,x3V,x0H),pointsto(x5V,x4V,x2H),store(x3V,x1F,x4V)),
    Rule(88,FValue(0.5,Token(88)),pointsto(x2V,x5V,x0H),load(x3V,x4F,x5V),pointsto(x2V,x3V,x0H),receiver_formal(x0H,x1V)),
    Rule(89,FValue(0.5,Token(89)),pointsto(x0V,x3V,x1H),invocation(x1H,x2M),points_initial(x0V,x1H),receiver_actual(x2M,x3V)),
    Rule(90,FValue(0.5,Token(90)),pointsto(x3V,x0V,x1H),invocation(x1H,x2M),points_initial(x0V,x1H),receiver_actual(x2M,x3V)),
    Rule(91,FValue(0.5,Token(91)),pointsto(x0V,x4V,x1H),formal(x2M,x3Z,x4V),invocation(x1H,x2M),points_initial(x0V,x1H)),
    Rule(92,FValue(0.5,Token(92)),pointsto(x0V,x4V,x1H),points_initial(x0V,x1H),receiver_formal(x1H,x2V),store(x2V,x3F,x4V)),
    Rule(93,FValue(0.5,Token(93)),pointsto(x0V,x4V,x1H),load(x2V,x3F,x4V),points_initial(x0V,x1H),receiver_formal(x1H,x2V)),
    Rule(94,FValue(0.5,Token(94)),pointsto(x0V,x2V,x1H),points_initial(x0V,x1H),points_initial(x2V,x1H),receiver_actual(x3M,x2V)),
    Rule(95,FValue(0.5,Token(95)),pointsto(x0V,x2V,x1H),points_initial(x0V,x1H),points_initial(x2V,x1H),receiver_formal(x3H,x2V)),
    Rule(96,FValue(0.5,Token(96)),pointsto(x2V,x0V,x1H),points_initial(x0V,x1H),points_initial(x2V,x1H),receiver_actual(x3M,x2V)),
    Rule(97,FValue(0.5,Token(97)),pointsto(x2V,x0V,x1H),points_initial(x0V,x1H),points_initial(x2V,x1H),receiver_formal(x3H,x2V)),
    Rule(98,FValue(0.5,Token(98)),pointsto(x0V,x3V,x1H),points_initial(x0V,x1H),points_initial(x0V,x2H),receiver_formal(x2H,x3V)),
    Rule(99,FValue(0.5,Token(99)),pointsto(x3V,x0V,x1H),points_initial(x0V,x1H),points_initial(x0V,x2H),receiver_formal(x2H,x3V)),
    Rule(100,FValue(0.5,Token(100)),pointsto(x0V,x3V,x1H),points_initial(x0V,x1H),points_initial(x3V,x2H),receiver_formal(x2H,x0V))
  )

  def soup3: Set[Rule[FValue]] = Set(
    Rule(101,FValue(0.5,Token(101)),pointsto(x0V,x3V,x2H),points_initial(x0V,x1H),points_initial(x3V,x2H),receiver_formal(x2H,x0V)),
    Rule(102,FValue(0.5,Token(102)),pointsto(x3V,x0V,x1H),points_initial(x0V,x1H),points_initial(x3V,x2H),receiver_formal(x2H,x0V)),
    Rule(103,FValue(0.5,Token(103)),pointsto(x3V,x0V,x2H),points_initial(x0V,x1H),points_initial(x3V,x2H),receiver_formal(x2H,x0V)),
    Rule(104,FValue(0.5,Token(104)),pointsto(x0V,x2V,x3H),points_initial(x0V,x1H),receiver_formal(x1H,x2V),receiver_formal(x3H,x2V)),
    Rule(105,FValue(0.5,Token(105)),pointsto(x0V,x3V,x1H),points_initial(x0V,x1H),pointsto(x3V,x4V,x2H),receiver_formal(x2H,x0V)),
    Rule(106,FValue(0.5,Token(106)),pointsto(x0V,x4V,x1H),points_initial(x0V,x1H),pointsto(x3V,x4V,x2H),receiver_formal(x2H,x0V)),
    Rule(107,FValue(0.5,Token(107)),pointsto(x3V,x0V,x1H),points_initial(x0V,x1H),pointsto(x3V,x4V,x2H),receiver_formal(x2H,x0V)),
    Rule(120,FValue(0.5,Token(120)),pointsto(x0V,x4V,x1H),actual(x2H,x3Z,x0V),points_initial(x0V,x1H),points_initial(x4V,x2H)),
    Rule(121,FValue(0.5,Token(121)),pointsto(x4V,x0V,x1H),actual(x2H,x3Z,x0V),points_initial(x0V,x1H),points_initial(x4V,x2H)),
    Rule(123,FValue(0.5,Token(123)),pointsto(x0V,x5V,x1H),heappointsto(x1H,x2F,x3H),load(x4V,x2F,x5V),points_initial(x0V,x1H)),
    Rule(124,FValue(0.5,Token(124)),pointsto(x0V,x5V,x1H),actual(x1H,x2Z,x3V),formal(x4M,x2Z,x5V),points_initial(x0V,x1H)),
    Rule(125,FValue(0.5,Token(125)),pointsto(x0V,x5V,x1H),actual(x1H,x2Z,x3V),actual(x4H,x2Z,x5V),points_initial(x0V,x1H)),
    Rule(126,FValue(0.5,Token(126)),pointsto(x0V,x3V,x4H),actual(x1H,x2Z,x3V),actual(x4H,x2Z,x5V),points_initial(x0V,x1H)),
    Rule(142,FValue(0.5,Token(142)),pointsto(x4V,x0V,x1H),load(x2V,x3F,x0V),points_initial(x0V,x1H),pointsto(x4V,x2V,x5H)),
    Rule(143,FValue(0.5,Token(143)),pointsto(x2V,x6V,x1H),assign(x2V,x3V,x0V,x4V),points_initial(x0V,x1H),store(x3V,x5F,x6V)),
    Rule(144,FValue(0.5,Token(144)),pointsto(x2V,x6V,x1H),assign(x2V,x3V,x0V,x4V),load(x3V,x5F,x6V),points_initial(x0V,x1H)),
    Rule(145,FValue(0.5,Token(145)),pointsto(x2V,x6V,x1H),assign(x2V,x3V,x4V,x0V),points_initial(x0V,x1H),store(x3V,x5F,x6V)),
    Rule(146,FValue(0.5,Token(146)),pointsto(x2V,x6V,x1H),assign(x2V,x3V,x4V,x0V),load(x3V,x5F,x6V),points_initial(x0V,x1H)),
    Rule(147,FValue(0.5,Token(147)),pointsto(x2V,x6V,x1H),assign(x2V,x3V,x4V,x0V),points_initial(x0V,x1H),store(x4V,x5F,x6V)),
    Rule(148,FValue(0.5,Token(148)),pointsto(x2V,x6V,x1H),assign(x2V,x3V,x4V,x0V),load(x4V,x5F,x6V),points_initial(x0V,x1H)),
    Rule(149,FValue(0.5,Token(149)),pointsto(x3V,x6V,x1H),assign(x2V,x3V,x4V,x0V),points_initial(x0V,x1H),store(x4V,x5F,x6V)),
    Rule(150,FValue(0.5,Token(150)),pointsto(x3V,x6V,x1H),assign(x2V,x3V,x4V,x0V),load(x4V,x5F,x6V),points_initial(x0V,x1H))
  )

  def soup4: Set[Rule[FValue]] = Set(
    Rule(151,FValue(0.5,Token(151)),pointsto(x3V,x5V,x1H),assign(x0V,x2V,x3V,x4V),points_initial(x0V,x1H),pointsto(x2V,x5V,x6H)),
    Rule(152,FValue(0.5,Token(152)),pointsto(x4V,x5V,x1H),assign(x0V,x2V,x3V,x4V),points_initial(x0V,x1H),pointsto(x2V,x5V,x6H)),
    Rule(153,FValue(0.5,Token(153)),pointsto(x4V,x5V,x1H),assign(x0V,x2V,x3V,x4V),points_initial(x0V,x1H),pointsto(x3V,x5V,x6H)),
    Rule(154,FValue(0.5,Token(154)),pointsto(x5V,x3V,x1H),assign(x2V,x0V,x3V,x4V),points_initial(x0V,x1H),pointsto(x5V,x2V,x6H)),
    Rule(155,FValue(0.5,Token(155)),pointsto(x5V,x4V,x1H),assign(x2V,x0V,x3V,x4V),points_initial(x0V,x1H),pointsto(x5V,x2V,x6H)),
    Rule(156,FValue(0.5,Token(156)),pointsto(x4V,x5V,x1H),assign(x2V,x0V,x3V,x4V),points_initial(x0V,x1H),pointsto(x3V,x5V,x6H)),
    Rule(157,FValue(0.5,Token(157)),pointsto(x5V,x4V,x1H),assign(x2V,x3V,x0V,x4V),points_initial(x0V,x1H),pointsto(x5V,x2V,x6H)),
    Rule(158,FValue(0.5,Token(158)),pointsto(x5V,x4V,x1H),assign(x2V,x3V,x0V,x4V),points_initial(x0V,x1H),pointsto(x5V,x3V,x6H)),
    Rule(159,FValue(0.5,Token(159)),pointsto(x0V,x4V,x1H),heappointsto(x1H,x2F,x3H),points_initial(x0V,x1H),receiver_formal(x3H,x4V)),
    Rule(160,FValue(0.5,Token(160)),pointsto(x0V,x4V,x3H),heappointsto(x1H,x2F,x3H),points_initial(x0V,x1H),receiver_formal(x3H,x4V)),
    Rule(161,FValue(0.5,Token(161)),pointsto(x5V,x0V,x1H),actual(x2H,x4Z,x5V),heappointsto(x2H,x3F,x1H),points_initial(x0V,x1H)),
    Rule(162,FValue(0.5,Token(162)),pointsto(x5V,x0V,x1H),points_initial(x0V,x1H),pointsto(x2V,x3V,x1H),store(x2V,x4F,x5V)),
    Rule(163,FValue(0.5,Token(163)),pointsto(x5V,x0V,x1H),load(x2V,x4F,x5V),points_initial(x0V,x1H),pointsto(x2V,x3V,x1H)),
    Rule(164,FValue(0.5,Token(164)),pointsto(x0V,x4V,x3H),heappointsto(x1H,x2F,x3H),points_initial(x0V,x1H),store(x4V,x2F,x5V)),
    Rule(165,FValue(0.5,Token(165)),pointsto(x0V,x4V,x3H),heappointsto(x1H,x2F,x3H),load(x4V,x2F,x5V),points_initial(x0V,x1H)),
    Rule(166,FValue(0.5,Token(166)),pointsto(x0V,x5V,x3H),heappointsto(x1H,x2F,x3H),points_initial(x0V,x1H),store(x4V,x2F,x5V)),
    Rule(167,FValue(0.5,Token(167)),pointsto(x0V,x5V,x3H),heappointsto(x1H,x2F,x3H),load(x4V,x2F,x5V),points_initial(x0V,x1H)),
    Rule(168,FValue(0.5,Token(168)),pointsto(x0V,x5V,x1H),heappointsto(x2H,x3F,x1H),points_initial(x0V,x1H),store(x4V,x3F,x5V)),
    Rule(169,FValue(0.5,Token(169)),pointsto(x0V,x5V,x1H),heappointsto(x2H,x3F,x1H),load(x4V,x3F,x5V),points_initial(x0V,x1H)),
    Rule(170,FValue(0.5,Token(170)),pointsto(x0V,x5V,x1H),points_initial(x0V,x1H),pointsto(x0V,x2V,x3H),store(x2V,x4F,x5V)),
    Rule(171,FValue(0.5,Token(171)),pointsto(x0V,x5V,x1H),load(x2V,x4F,x5V),points_initial(x0V,x1H),pointsto(x0V,x2V,x3H)),
    Rule(172,FValue(0.5,Token(172)),pointsto(x5V,x0V,x3H),points_initial(x0V,x1H),pointsto(x2V,x0V,x3H),store(x2V,x4F,x5V)),
    Rule(173,FValue(0.5,Token(173)),pointsto(x5V,x0V,x3H),load(x2V,x4F,x5V),points_initial(x0V,x1H),pointsto(x2V,x0V,x3H)),
    Rule(174,FValue(0.5,Token(174)),pointsto(x4V,x0V,x3H),points_initial(x0V,x1H),pointsto(x2V,x0V,x3H),store(x4V,x5F,x2V)),
    Rule(175,FValue(0.5,Token(175)),pointsto(x4V,x0V,x3H),load(x4V,x5F,x2V),points_initial(x0V,x1H),pointsto(x2V,x0V,x3H)),
    Rule(176,FValue(0.5,Token(176)),pointsto(x2V,x5V,x1H),points_initial(x0V,x1H),pointsto(x2V,x3V,x1H),store(x3V,x4F,x5V)),
    Rule(177,FValue(0.5,Token(177)),pointsto(x2V,x5V,x1H),load(x3V,x4F,x5V),points_initial(x0V,x1H),pointsto(x2V,x3V,x1H)),
    Rule(178,FValue(0.5,Token(178)),pointsto(x2V,x0V,x5H),heappointsto(x3H,x4F,x5H),points_initial(x0V,x1H),pointsto(x2V,x0V,x3H)),
    Rule(193,FValue(0.5,Token(193)),pointsto(x1V,x3V,x5H),heappointsto(x4H,x2F,x5H),receiver_formal(x0H,x1V),store(x1V,x2F,x3V)),
    Rule(194,FValue(0.5,Token(194)),pointsto(x1V,x3V,x5H),heappointsto(x4H,x2F,x5H),load(x1V,x2F,x3V),receiver_formal(x0H,x1V)),
    Rule(195,FValue(0.5,Token(195)),pointsto(x5V,x1V,x3H),pointsto(x2V,x1V,x3H),receiver_actual(x0M,x1V),store(x2V,x4F,x5V)),
    Rule(196,FValue(0.5,Token(196)),pointsto(x5V,x1V,x3H),load(x2V,x4F,x5V),pointsto(x2V,x1V,x3H),receiver_actual(x0M,x1V)),
    Rule(197,FValue(0.5,Token(197)),pointsto(x5V,x1V,x3H),pointsto(x2V,x1V,x3H),receiver_formal(x0H,x1V),store(x2V,x4F,x5V)),
    Rule(198,FValue(0.5,Token(198)),pointsto(x5V,x1V,x3H),load(x2V,x4F,x5V),pointsto(x2V,x1V,x3H),receiver_formal(x0H,x1V)),
    Rule(199,FValue(0.5,Token(199)),pointsto(x4V,x1V,x3H),pointsto(x2V,x1V,x3H),receiver_actual(x0M,x1V),store(x4V,x5F,x2V)),
    Rule(200,FValue(0.5,Token(200)),pointsto(x4V,x1V,x3H),load(x4V,x5F,x2V),pointsto(x2V,x1V,x3H),receiver_actual(x0M,x1V))
  )

  def soup5: Set[Rule[FValue]] = Set(
    Rule(201,FValue(0.5,Token(201)),pointsto(x4V,x1V,x3H),pointsto(x2V,x1V,x3H),receiver_formal(x0H,x1V),store(x4V,x5F,x2V)),
    Rule(202,FValue(0.5,Token(202)),pointsto(x4V,x1V,x3H),load(x4V,x5F,x2V),pointsto(x2V,x1V,x3H),receiver_formal(x0H,x1V)),
    Rule(203,FValue(0.5,Token(203)),pointsto(x2V,x1V,x5H),heappointsto(x3H,x4F,x5H),pointsto(x2V,x1V,x3H),receiver_actual(x0M,x1V)),
    Rule(204,FValue(0.5,Token(204)),pointsto(x2V,x1V,x5H),heappointsto(x3H,x4F,x5H),pointsto(x2V,x1V,x3H),receiver_formal(x0H,x1V)),
    Rule(205,FValue(0.5,Token(205)),pointsto(x2V,x1V,x3H),points_initial(x2V,x0H),receiver_formal(x0H,x1V),receiver_formal(x3H,x2V)),
    Rule(206,FValue(0.5,Token(206)),pointsto(x3V,x1V,x0H),points_initial(x2V,x0H),receiver_formal(x0H,x1V),store(x3V,x4F,x2V)),
    Rule(207,FValue(0.5,Token(207)),pointsto(x3V,x1V,x0H),load(x3V,x4F,x2V),points_initial(x2V,x0H),receiver_formal(x0H,x1V)),
    Rule(208,FValue(0.5,Token(208)),pointsto(x3V,x1V,x0H),points_initial(x2V,x0H),pointsto(x3V,x2V,x4H),receiver_formal(x0H,x1V)),
    Rule(209,FValue(0.5,Token(209)),pointsto(x3V,x1V,x4H),points_initial(x2V,x0H),pointsto(x3V,x2V,x4H),receiver_formal(x0H,x1V)),
    Rule(210,FValue(0.5,Token(210)),pointsto(x5V,x1V,x0H),actual(x2H,x4Z,x5V),heappointsto(x2H,x3F,x0H),receiver_formal(x0H,x1V)),
    Rule(211,FValue(0.5,Token(211)),pointsto(x5V,x1V,x0H),pointsto(x2V,x3V,x0H),receiver_formal(x0H,x1V),store(x2V,x4F,x5V)),
    Rule(212,FValue(0.5,Token(212)),pointsto(x5V,x1V,x0H),load(x2V,x4F,x5V),pointsto(x2V,x3V,x0H),receiver_formal(x0H,x1V)),
    Rule(213,FValue(0.5,Token(213)),pointsto(x1V,x5V,x3H),heappointsto(x0H,x2F,x3H),pointsto(x4V,x5V,x3H),receiver_formal(x0H,x1V)),
    Rule(214,FValue(0.5,Token(214)),pointsto(x4V,x1V,x0H),heappointsto(x2H,x3F,x0H),pointsto(x4V,x5V,x2H),receiver_formal(x0H,x1V)),
    Rule(215,FValue(0.5,Token(215)),pointsto(x4V,x1V,x0H),pointsto(x2V,x3V,x0H),receiver_formal(x0H,x1V),store(x4V,x5F,x2V)),
    Rule(216,FValue(0.5,Token(216)),pointsto(x4V,x1V,x0H),load(x4V,x5F,x2V),pointsto(x2V,x3V,x0H),receiver_formal(x0H,x1V)),
    Rule(217,FValue(0.5,Token(217)),pointsto(x4V,x2V,x5H),formal(x0M,x1Z,x2V),pointsto(x3V,x4V,x5H),receiver_actual(x0M,x3V)),
    Rule(218,FValue(0.5,Token(218)),pointsto(x4V,x2V,x5H),actual(x0H,x1Z,x2V),pointsto(x3V,x4V,x5H),receiver_formal(x0H,x3V)),
    Rule(219,FValue(0.5,Token(219)),pointsto(x4V,x2V,x6H),actual(x3H,x1Z,x4V),formal(x0M,x1Z,x2V),heappointsto(x3H,x5F,x6H)),
    Rule(220,FValue(0.5,Token(220)),pointsto(x4V,x2V,x6H),actual(x0H,x1Z,x2V),actual(x3H,x1Z,x4V),heappointsto(x3H,x5F,x6H)),
    Rule(221,FValue(0.5,Token(221)),pointsto(x4V,x2V,x6H),pointsto(x3V,x5V,x6H),store(x0V,x1F,x2V),store(x3V,x1F,x4V)),
    Rule(222,FValue(0.5,Token(222)),pointsto(x4V,x2V,x6H),load(x3V,x1F,x4V),pointsto(x3V,x5V,x6H),store(x0V,x1F,x2V)),
    Rule(223,FValue(0.5,Token(223)),pointsto(x4V,x2V,x6H),load(x0V,x1F,x2V),pointsto(x3V,x5V,x6H),store(x3V,x1F,x4V)),
    Rule(224,FValue(0.5,Token(224)),pointsto(x4V,x2V,x6H),load(x0V,x1F,x2V),load(x3V,x1F,x4V),pointsto(x3V,x5V,x6H)),
    Rule(225,FValue(0.5,Token(225)),pointsto(x5V,x2V,x3H),actual(x3H,x1Z,x4V),formal(x0M,x1Z,x2V),pointsto(x5V,x6V,x3H)),
    Rule(226,FValue(0.5,Token(226)),pointsto(x5V,x2V,x3H),actual(x0H,x1Z,x2V),actual(x3H,x1Z,x4V),pointsto(x5V,x6V,x3H)),
    Rule(227,FValue(0.5,Token(227)),pointsto(x5V,x2V,x3H),heappointsto(x3H,x1F,x4H),pointsto(x5V,x6V,x3H),store(x0V,x1F,x2V)),
    Rule(228,FValue(0.5,Token(228)),pointsto(x5V,x2V,x3H),heappointsto(x3H,x1F,x4H),load(x0V,x1F,x2V),pointsto(x5V,x6V,x3H)),
    Rule(229,FValue(0.5,Token(229)),pointsto(x6V,x2V,x4H),formal(x0M,x1Z,x2V),pointsto(x3V,x2V,x4H),store(x3V,x5F,x6V)),
    Rule(230,FValue(0.5,Token(230)),pointsto(x6V,x2V,x4H),formal(x0M,x1Z,x2V),load(x3V,x5F,x6V),pointsto(x3V,x2V,x4H)),
    Rule(231,FValue(0.5,Token(231)),pointsto(x6V,x2V,x4H),actual(x0H,x1Z,x2V),pointsto(x3V,x2V,x4H),store(x3V,x5F,x6V)),
    Rule(232,FValue(0.5,Token(232)),pointsto(x6V,x2V,x4H),actual(x0H,x1Z,x2V),load(x3V,x5F,x6V),pointsto(x3V,x2V,x4H)),
    Rule(233,FValue(0.5,Token(233)),pointsto(x6V,x2V,x4H),pointsto(x3V,x2V,x4H),store(x0V,x1F,x2V),store(x3V,x5F,x6V)),
    Rule(234,FValue(0.5,Token(234)),pointsto(x6V,x2V,x4H),load(x3V,x5F,x6V),pointsto(x3V,x2V,x4H),store(x0V,x1F,x2V)),
    Rule(235,FValue(0.5,Token(235)),pointsto(x6V,x2V,x4H),load(x0V,x1F,x2V),pointsto(x3V,x2V,x4H),store(x3V,x5F,x6V)),
    Rule(236,FValue(0.5,Token(236)),pointsto(x6V,x2V,x4H),load(x0V,x1F,x2V),load(x3V,x5F,x6V),pointsto(x3V,x2V,x4H)),
    Rule(237,FValue(0.5,Token(237)),pointsto(x5V,x2V,x4H),formal(x0M,x1Z,x2V),pointsto(x3V,x2V,x4H),store(x5V,x6F,x3V)),
    Rule(238,FValue(0.5,Token(238)),pointsto(x5V,x2V,x4H),formal(x0M,x1Z,x2V),load(x5V,x6F,x3V),pointsto(x3V,x2V,x4H)),
    Rule(239,FValue(0.5,Token(239)),pointsto(x5V,x2V,x4H),actual(x0H,x1Z,x2V),pointsto(x3V,x2V,x4H),store(x5V,x6F,x3V)),
    Rule(240,FValue(0.5,Token(240)),pointsto(x5V,x2V,x4H),actual(x0H,x1Z,x2V),load(x5V,x6F,x3V),pointsto(x3V,x2V,x4H)),
    Rule(241,FValue(0.5,Token(241)),pointsto(x5V,x2V,x4H),pointsto(x3V,x2V,x4H),store(x0V,x1F,x2V),store(x5V,x6F,x3V)),
    Rule(242,FValue(0.5,Token(242)),pointsto(x5V,x2V,x4H),load(x5V,x6F,x3V),pointsto(x3V,x2V,x4H),store(x0V,x1F,x2V)),
    Rule(243,FValue(0.5,Token(243)),pointsto(x5V,x2V,x4H),load(x0V,x1F,x2V),pointsto(x3V,x2V,x4H),store(x5V,x6F,x3V)),
    Rule(244,FValue(0.5,Token(244)),pointsto(x5V,x2V,x4H),load(x0V,x1F,x2V),load(x5V,x6F,x3V),pointsto(x3V,x2V,x4H)),
    Rule(245,FValue(0.5,Token(245)),pointsto(x3V,x2V,x6H),formal(x0M,x1Z,x2V),heappointsto(x4H,x5F,x6H),pointsto(x3V,x2V,x4H)),
    Rule(246,FValue(0.5,Token(246)),pointsto(x3V,x2V,x6H),actual(x0H,x1Z,x2V),heappointsto(x4H,x5F,x6H),pointsto(x3V,x2V,x4H)),
    Rule(247,FValue(0.5,Token(247)),pointsto(x3V,x2V,x6H),heappointsto(x4H,x5F,x6H),pointsto(x3V,x2V,x4H),store(x0V,x1F,x2V)),
    Rule(248,FValue(0.5,Token(248)),pointsto(x3V,x2V,x6H),heappointsto(x4H,x5F,x6H),load(x0V,x1F,x2V),pointsto(x3V,x2V,x4H)),
    Rule(249,FValue(0.5,Token(249)),pointsto(x4V,x2V,x0H),actual(x0H,x1Z,x2V),points_initial(x3V,x0H),store(x4V,x5F,x3V)),
    Rule(250,FValue(0.5,Token(250)),pointsto(x4V,x2V,x0H),actual(x0H,x1Z,x2V),load(x4V,x5F,x3V),points_initial(x3V,x0H))
  )

  def soup6: Set[Rule[FValue]] = Set(
    Rule(251,FValue(0.5,Token(251)),pointsto(x3V,x2V,x5H),actual(x0H,x1Z,x2V),points_initial(x3V,x0H),pointsto(x4V,x3V,x5H)),
    Rule(252,FValue(0.5,Token(252)),pointsto(x5V,x2V,x0H),actual(x0H,x1Z,x2V),actual(x3H,x1Z,x4V),pointsto(x5V,x6V,x3H)),
    Rule(253,FValue(0.5,Token(253)),pointsto(x2V,x5V,x4H),actual(x0H,x1Z,x2V),heappointsto(x0H,x3F,x4H),store(x5V,x3F,x6V)),
    Rule(254,FValue(0.5,Token(254)),pointsto(x2V,x5V,x4H),actual(x0H,x1Z,x2V),heappointsto(x0H,x3F,x4H),load(x5V,x3F,x6V)),
    Rule(255,FValue(0.5,Token(255)),pointsto(x2V,x5V,x4H),pointsto(x0V,x3V,x4H),pointsto(x5V,x3V,x6H),store(x0V,x1F,x2V)),
    Rule(256,FValue(0.5,Token(256)),pointsto(x2V,x5V,x4H),load(x0V,x1F,x2V),pointsto(x0V,x3V,x4H),pointsto(x5V,x3V,x6H)),
    Rule(257,FValue(0.5,Token(257)),pointsto(x2V,x6V,x4H),actual(x0H,x1Z,x2V),heappointsto(x0H,x3F,x4H),pointsto(x5V,x6V,x4H)),
    Rule(258,FValue(0.5,Token(258)),pointsto(x2V,x6V,x4H),pointsto(x0V,x3V,x4H),pointsto(x5V,x6V,x4H),store(x0V,x1F,x2V)),
    Rule(259,FValue(0.5,Token(259)),pointsto(x2V,x6V,x4H),load(x0V,x1F,x2V),pointsto(x0V,x3V,x4H),pointsto(x5V,x6V,x4H)),
    Rule(260,FValue(0.5,Token(260)),pointsto(x5V,x2V,x0H),actual(x0H,x1Z,x2V),heappointsto(x3H,x4F,x0H),pointsto(x5V,x6V,x3H)),
    Rule(261,FValue(0.5,Token(261)),pointsto(x5V,x2V,x0H),actual(x0H,x1Z,x2V),pointsto(x3V,x4V,x0H),store(x5V,x6F,x3V)),
    Rule(262,FValue(0.5,Token(262)),pointsto(x5V,x2V,x0H),actual(x0H,x1Z,x2V),load(x5V,x6F,x3V),pointsto(x3V,x4V,x0H)),
    Rule(288,FValue(0.5,Token(288)),pointsto(x3V,x2V,x6H),load(x0V,x1F,x2V),load(x3V,x1F,x4V),pointsto(x4V,x5V,x6H)),
    Rule(289,FValue(0.5,Token(289)),pointsto(x3V,x2V,x7H),assign(x3V,x0V,x4V,x5V),pointsto(x4V,x6V,x7H),store(x0V,x1F,x2V)),
    Rule(290,FValue(0.5,Token(290)),pointsto(x3V,x2V,x7H),assign(x3V,x0V,x4V,x5V),load(x0V,x1F,x2V),pointsto(x4V,x6V,x7H)),
    Rule(291,FValue(0.5,Token(291)),pointsto(x3V,x2V,x7H),assign(x3V,x0V,x4V,x5V),pointsto(x5V,x6V,x7H),store(x0V,x1F,x2V)),
    Rule(292,FValue(0.5,Token(292)),pointsto(x3V,x2V,x7H),assign(x3V,x0V,x4V,x5V),load(x0V,x1F,x2V),pointsto(x5V,x6V,x7H)),
    Rule(293,FValue(0.5,Token(293)),pointsto(x3V,x2V,x7H),assign(x3V,x4V,x0V,x5V),pointsto(x5V,x6V,x7H),store(x0V,x1F,x2V)),
    Rule(294,FValue(0.5,Token(294)),pointsto(x3V,x2V,x7H),assign(x3V,x4V,x0V,x5V),load(x0V,x1F,x2V),pointsto(x5V,x6V,x7H)),
    Rule(295,FValue(0.5,Token(295)),pointsto(x4V,x2V,x7H),assign(x3V,x4V,x0V,x5V),pointsto(x5V,x6V,x7H),store(x0V,x1F,x2V)),
    Rule(296,FValue(0.5,Token(296)),pointsto(x4V,x2V,x7H),assign(x3V,x4V,x0V,x5V),load(x0V,x1F,x2V),pointsto(x5V,x6V,x7H)),
    Rule(297,FValue(0.5,Token(297)),pointsto(x5V,x0V,x4H),pointsto(x0V,x1V,x2H),pointsto(x3V,x1V,x4H),store(x5V,x6F,x3V)),
    Rule(298,FValue(0.5,Token(298)),pointsto(x5V,x0V,x4H),load(x5V,x6F,x3V),pointsto(x0V,x1V,x2H),pointsto(x3V,x1V,x4H)),
    Rule(299,FValue(0.5,Token(299)),pointsto(x5V,x0V,x4H),heappointsto(x3H,x1F,x4H),pointsto(x5V,x6V,x3H),store(x0V,x1F,x2V)),
    Rule(300,FValue(0.5,Token(300)),pointsto(x5V,x0V,x4H),heappointsto(x3H,x1F,x4H),load(x0V,x1F,x2V),pointsto(x5V,x6V,x3H))
  )

  def soup7: Set[Rule[FValue]] = Set(
    Rule(301,FValue(0.5,Token(301)),pointsto(x5V,x2V,x4H),heappointsto(x3H,x1F,x4H),pointsto(x5V,x6V,x3H),store(x0V,x1F,x2V)),
    Rule(302,FValue(0.5,Token(302)),pointsto(x5V,x2V,x4H),heappointsto(x3H,x1F,x4H),load(x0V,x1F,x2V),pointsto(x5V,x6V,x3H)),
    Rule(303,FValue(0.5,Token(303)),pointsto(x6V,x2V,x4H),heappointsto(x3H,x1F,x4H),pointsto(x5V,x6V,x3H),store(x0V,x1F,x2V)),
    Rule(304,FValue(0.5,Token(304)),pointsto(x6V,x2V,x4H),heappointsto(x3H,x1F,x4H),load(x0V,x1F,x2V),pointsto(x5V,x6V,x3H)),
    Rule(305,FValue(0.5,Token(305)),pointsto(x5V,x2V,x4H),heappointsto(x3H,x1F,x4H),pointsto(x5V,x6V,x4H),store(x0V,x1F,x2V)),
    Rule(306,FValue(0.5,Token(306)),pointsto(x5V,x2V,x4H),heappointsto(x3H,x1F,x4H),load(x0V,x1F,x2V),pointsto(x5V,x6V,x4H)),
    Rule(307,FValue(0.5,Token(307)),pointsto(x2V,x5V,x4H),pointsto(x0V,x3V,x4H),store(x0V,x1F,x2V),store(x5V,x6F,x3V)),
    Rule(308,FValue(0.5,Token(308)),pointsto(x2V,x5V,x4H),load(x5V,x6F,x3V),pointsto(x0V,x3V,x4H),store(x0V,x1F,x2V)),
    Rule(345,FValue(0.5,Token(345)),pointsto(x0V,x5V,x7H),assign(x0V,x1V,x2V,x3V),heappointsto(x6H,x4F,x7H),load(x1V,x4F,x5V)),
    Rule(346,FValue(0.5,Token(346)),pointsto(x0V,x5V,x7H),assign(x0V,x1V,x2V,x3V),heappointsto(x6H,x4F,x7H),store(x2V,x4F,x5V)),
    Rule(347,FValue(0.5,Token(347)),pointsto(x0V,x5V,x7H),assign(x0V,x1V,x2V,x3V),heappointsto(x6H,x4F,x7H),load(x2V,x4F,x5V)),
    Rule(348,FValue(0.5,Token(348)),pointsto(x1V,x5V,x7H),assign(x0V,x1V,x2V,x3V),heappointsto(x6H,x4F,x7H),store(x2V,x4F,x5V)),
    Rule(349,FValue(0.5,Token(349)),pointsto(x1V,x5V,x7H),assign(x0V,x1V,x2V,x3V),heappointsto(x6H,x4F,x7H),load(x2V,x4F,x5V)),
    Rule(350,FValue(0.5,Token(350)),pointsto(x4V,x2V,x7H),assign(x0V,x1V,x2V,x3V),heappointsto(x5H,x6F,x7H),pointsto(x4V,x0V,x5H))
  )

  def soup8: Set[Rule[FValue]] = Set(
    Rule(351,FValue(0.5,Token(351)),pointsto(x4V,x3V,x7H),assign(x0V,x1V,x2V,x3V),heappointsto(x5H,x6F,x7H),pointsto(x4V,x0V,x5H)),
    Rule(352,FValue(0.5,Token(352)),pointsto(x4V,x3V,x7H),assign(x0V,x1V,x2V,x3V),heappointsto(x5H,x6F,x7H),pointsto(x4V,x1V,x5H)),
    Rule(353,FValue(0.5,Token(353)),pointsto(x4V,x5V,x2H),actual(x0H,x3Z,x4V),heappointsto(x0H,x1F,x2H),pointsto(x4V,x5V,x6H)),
    Rule(354,FValue(0.5,Token(354)),pointsto(x4V,x5V,x2H),pointsto(x0V,x1V,x2H),pointsto(x4V,x5V,x6H),store(x0V,x3F,x4V)),
    Rule(355,FValue(0.5,Token(355)),pointsto(x4V,x5V,x2H),load(x0V,x3F,x4V),pointsto(x0V,x1V,x2H),pointsto(x4V,x5V,x6H)),
    Rule(356,FValue(0.5,Token(356)),pointsto(x3V,x4V,x2H),heappointsto(x0H,x1F,x2H),pointsto(x5V,x3V,x6H),store(x3V,x1F,x4V)),
    Rule(357,FValue(0.5,Token(357)),pointsto(x3V,x4V,x2H),heappointsto(x0H,x1F,x2H),load(x3V,x1F,x4V),pointsto(x5V,x3V,x6H)),
    Rule(380,FValue(0.5,Token(380)),pointsto(x0V,x5V,x2H),pointsto(x0V,x1V,x2H),pointsto(x0V,x3V,x4H),pointsto(x3V,x5V,x6H)),
    Rule(381,FValue(0.5,Token(381)),pointsto(x1V,x5V,x2H),pointsto(x0V,x1V,x2H),pointsto(x0V,x3V,x4H),pointsto(x3V,x5V,x6H)),
    Rule(382,FValue(0.5,Token(382)),pointsto(x3V,x5V,x2H),pointsto(x0V,x1V,x2H),pointsto(x0V,x3V,x4H),pointsto(x3V,x5V,x6H)),
    Rule(383,FValue(0.5,Token(383)),pointsto(x0V,x3V,x6H),pointsto(x0V,x1V,x2H),pointsto(x1V,x3V,x4H),pointsto(x5V,x3V,x6H)),
    Rule(384,FValue(0.5,Token(384)),pointsto(x0V,x5V,x6H),pointsto(x0V,x1V,x2H),pointsto(x1V,x3V,x4H),pointsto(x5V,x3V,x6H)),
    Rule(386,FValue(0.5,Token(386)),pointsto(x0V,x1V,x2H),heappointsto(x5H,x4F,x2H),load(x3V,x4F,x1V),pointsto(x0V,x3V,x5H)),
    Rule(387,FValue(0.5,Token(387)),pointsto(x0V,x1V,x2H),assign(x0V,x1V,x3V,x4V),pointsto(x3V,x4V,x2H))
  )

  override val soup: Set[Rule[FValue]] = soup1 ++ soup2 ++ soup3 ++ soup4 ++ soup5 ++ soup6 ++ soup7 ++ soup8

  override val expected: Set[Any] = Set(83, 90, 386, 387)
  override val maxVarCount: Int = 6
}
