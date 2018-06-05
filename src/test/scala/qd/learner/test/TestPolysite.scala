package qd
package learner

class TestPolysite extends Problem {
  override val name: String = "Polysite"

  val ISet = Range(0, 8).map(i => Atom(i)).toSet
  val I = Domain("I", ISet)

  val CSet = Range(0, 2).map(i => Atom(i)).toSet
  val C = Domain("C", CSet)

  val MSet = Range(0, 12).map(i => Atom(i)).toSet
  val M = Domain("M", MSet)

  val CICM : Relation = Relation("CICM", C, I, M)
  val virtIM = Relation("virtIM", I, M)
  val Mneq = Relation("Mneq", M, M)

  val insvIM = Relation("insvIM", I, M)
  val virtI = Relation("virtI", I)
  val polySite = Relation("polySite", I)

  val CICMTuples = Set((1, 1, 0), (0, 4, 5), (1, 6, 1), (0, 2, 6), (1, 1, 2), (0, 0, 11), (1, 1, 1), (0, 3, 1), (0, 2, 4)).map{case (a,b,c) => DTuple(Atom(a), Atom(b), Atom(c))}
  val virtIMTuples = Set((1, 3), (6, 7), (6, 1), (0, 4), (0, 11), (7, 10), (1, 7), (0, 9), (3, 7), (1, 0), (5, 7)).map{case(a,b) => DTuple(Atom(a), Atom(b))}
  val pairs_list = for (i <- 0 to 11; j <- 0 to 11) yield (i,j)
  val filtered : IndexedSeq[(Int, Int)] = pairs_list.filter { case (a,b) => a != b }
  val MneqTuples : Set[DTuple] = Set(filtered : _*).map{ case (a) => DTuple(Atom(a._1), Atom(a._2)) }

  val insvIMTuples = Set((1, 2), (2, 6), (4, 5), (6, 1), (3, 1), (1, 1), (0, 11), (1, 0), (2, 4)).map{case (a,b) => DTuple(Atom(a), Atom(b))}
  val virtITuples = Set(0,1,3,5,6,7).map{ case (x) => DTuple(Atom(x)) }
  val polysiteTuples = Set(2, 1).map{ case (x) => DTuple(Atom(x)) }

  override val edb : Config[FValue] = Config(
    CICM -> (Instance[FValue](CICM) ++ CICMTuples.map(t => t -> FValue.One).toMap),
    virtIM -> (Instance[FValue](virtIM) ++ virtIMTuples.map(t => t -> FValue.One).toMap),
    Mneq -> (Instance[FValue](Mneq) ++ MneqTuples.map(t => t -> FValue.One).toMap),
  )

  override val refOut : Config[FValue] = Config (
    insvIM -> (Instance[FValue](insvIM) ++ insvIMTuples.map(t => t -> FValue.One).toMap),
    virtI -> (Instance[FValue](virtI) ++ virtITuples.map(t => t -> FValue.One).toMap),
    polySite -> (Instance[FValue](polySite) ++ polysiteTuples.map(t => t -> FValue.One).toMap)
  )

  val x0I : Variable = Variable("x0I", I)
  val x1I : Variable = Variable("x1I", I)
  val x2I : Variable = Variable("x2I", I)
  val x3I : Variable = Variable("x3I", I)
  val x4I : Variable = Variable("x4I", I)
  val x5I : Variable = Variable("x5I", I)

  val x0M : Variable = Variable("x0M", M)
  val x1M : Variable = Variable("x1M", M)
  val x2M : Variable = Variable("x2M", M)
  val x3M : Variable = Variable("x3M", M)
  val x4M : Variable = Variable("x4M", M)
  val x5M : Variable = Variable("x5M", M)

  val x0C : Variable = Variable("x0C", C)
  val x1C : Variable = Variable("x1C", C)
  val x2C : Variable = Variable("x2C", C)
  val x3C : Variable = Variable("x3C", C)
  val x4C : Variable = Variable("x4C", C)

  // Expected: 2, 139, 160
  val soup_pre : Set[Rule[FValue]] = Set(
		Rule(1, FValue(0.038144, Token(1)), virtI(x0I),insvIM(x0I,x1M)),
		Rule(2, FValue(0.171454, Token(2)), virtI(x0I),virtIM(x0I,x1M)),
		Rule(3, FValue(0.010000, Token(3)), virtI(x1I),CICM(x0C,x1I,x2M)),
		Rule(4, FValue(0.492094, Token(4)), virtI(x0I),polySite(x0I)),
		Rule(5, FValue(0.990000, Token(5)), virtI(x3I),insvIM(x0I,x1M),insvIM(x3I,x2M),virtIM(x0I,x2M)),
		Rule(6, FValue(0.136393, Token(6)), virtI(x3I),insvIM(x0I,x1M),virtIM(x0I,x2M),virtIM(x3I,x2M)),
		Rule(7, FValue(0.087095, Token(7)), virtI(x3I),insvIM(x0I,x2M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
		Rule(8, FValue(0.536840, Token(8)), virtI(x3I),insvIM(x0I,x2M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
		Rule(9, FValue(0.099261, Token(9)), virtI(x3I),Mneq(x0M,x1M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
		Rule(10, FValue(0.229258, Token(10)), virtI(x3I),Mneq(x0M,x1M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
		Rule(11, FValue(0.997241, Token(11)), virtI(x3I),Mneq(x1M,x2M),insvIM(x0I,x1M),insvIM(x3I,x2M)),
		Rule(12, FValue(0.137142, Token(12)), virtI(x3I),Mneq(x1M,x2M),insvIM(x0I,x1M),virtIM(x3I,x2M)),
		Rule(13, FValue(0.320761, Token(13)), virtI(x3I),Mneq(x0M,x1M),Mneq(x1M,x2M),insvIM(x3I,x2M)),
		Rule(14, FValue(0.990000, Token(14)), virtI(x3I),Mneq(x0M,x1M),Mneq(x1M,x2M),virtIM(x3I,x2M)),
		Rule(15, FValue(0.559118, Token(15)), virtI(x3I),Mneq(x1M,x2M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
		Rule(16, FValue(0.357410, Token(16)), virtI(x3I),Mneq(x1M,x2M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
		Rule(17, FValue(0.010000, Token(17)), virtI(x3I),Mneq(x2M,x1M),insvIM(x0I,x1M),insvIM(x3I,x2M)),
		Rule(18, FValue(0.153879, Token(18)), virtI(x3I),Mneq(x2M,x1M),insvIM(x0I,x1M),virtIM(x3I,x2M)),
		Rule(19, FValue(0.367162, Token(19)), virtI(x3I),Mneq(x2M,x1M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
		Rule(20, FValue(0.410943, Token(20)), virtI(x3I),Mneq(x2M,x1M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
		Rule(21, FValue(0.109646, Token(21)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x2M,x0M)),
		Rule(22, FValue(0.170636, Token(22)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x0I,x1M)),
		Rule(23, FValue(0.019128, Token(23)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x1M,x2M)),
		Rule(24, FValue(0.953198, Token(24)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x0I,x1M)),
		Rule(25, FValue(0.000073, Token(25)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x0I,x1M)),
		Rule(26, FValue(0.355639, Token(26)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x0I,x1M)),
		Rule(27, FValue(0.010000, Token(27)), virtI(x2I),CICM(x3C,x2I,x4M),Mneq(x0M,x1M),insvIM(x2I,x0M)),
		Rule(28, FValue(0.248472, Token(28)), virtI(x2I),CICM(x3C,x2I,x4M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
		Rule(29, FValue(0.010000, Token(29)), virtI(x2I),Mneq(x0M,x1M),insvIM(x2I,x0M),polySite(x2I)),
		Rule(30, FValue(0.248652, Token(30)), virtI(x2I),Mneq(x0M,x1M),polySite(x2I),virtIM(x2I,x0M)),
		Rule(31, FValue(0.216333, Token(31)), virtI(x3I),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),insvIM(x3I,x4M)),
		Rule(32, FValue(0.190311, Token(32)), virtI(x3I),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),virtIM(x3I,x4M)),
		Rule(33, FValue(0.055548, Token(33)), virtI(x3I),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),polySite(x3I)),
		Rule(34, FValue(0.314261, Token(34)), virtI(x0I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x0I,x1M)),
		Rule(35, FValue(0.210520, Token(35)), virtI(x0I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x0I,x1M)),
		Rule(36, FValue(0.208094, Token(36)), virtI(x0I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x0I,x1M)),
		Rule(37, FValue(0.233512, Token(37)), virtI(x0I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x0I,x1M)),
		Rule(38, FValue(0.376434, Token(38)), virtI(x0I),CICM(x3C,x2I,x4M),insvIM(x0I,x1M),insvIM(x2I,x1M)),
		Rule(39, FValue(0.246172, Token(39)), virtI(x0I),CICM(x3C,x2I,x4M),insvIM(x0I,x1M),virtIM(x2I,x1M)),
		Rule(40, FValue(0.247143, Token(40)), virtI(x0I),CICM(x3C,x2I,x4M),insvIM(x2I,x1M),virtIM(x0I,x1M)),
		Rule(41, FValue(0.772139, Token(41)), virtI(x0I),CICM(x3C,x2I,x4M),virtIM(x0I,x1M),virtIM(x2I,x1M)),
		Rule(42, FValue(0.929549, Token(42)), virtI(x0I),insvIM(x0I,x1M),insvIM(x2I,x1M),virtI(x2I)),
		Rule(43, FValue(0.010000, Token(43)), virtI(x0I),insvIM(x0I,x1M),insvIM(x2I,x1M),polySite(x2I)),
		Rule(44, FValue(0.261521, Token(44)), virtI(x0I),insvIM(x0I,x1M),virtI(x2I),virtIM(x2I,x1M)),
		Rule(45, FValue(0.818272, Token(45)), virtI(x0I),insvIM(x0I,x1M),polySite(x2I),virtIM(x2I,x1M)),
		Rule(46, FValue(0.540108, Token(46)), virtI(x0I),insvIM(x2I,x1M),virtI(x2I),virtIM(x0I,x1M)),
		Rule(47, FValue(0.990000, Token(47)), virtI(x0I),insvIM(x2I,x1M),polySite(x2I),virtIM(x0I,x1M)),
		Rule(48, FValue(0.085864, Token(48)), virtI(x0I),virtI(x2I),virtIM(x0I,x1M),virtIM(x2I,x1M)),
		Rule(49, FValue(0.492156, Token(49)), virtI(x0I),polySite(x2I),virtIM(x0I,x1M),virtIM(x2I,x1M)),
		Rule(50, FValue(0.677008, Token(50)), virtI(x4I),CICM(x2C,x0I,x3M),insvIM(x0I,x1M),insvIM(x4I,x3M)),
		Rule(51, FValue(0.100689, Token(51)), virtI(x4I),CICM(x2C,x0I,x3M),insvIM(x0I,x1M),virtIM(x4I,x3M)),
		Rule(52, FValue(0.688809, Token(52)), virtI(x4I),CICM(x2C,x0I,x3M),insvIM(x4I,x3M),virtIM(x0I,x1M)),
		Rule(53, FValue(0.010000, Token(53)), virtI(x4I),CICM(x2C,x0I,x3M),virtIM(x0I,x1M),virtIM(x4I,x3M)),
		Rule(54, FValue(0.990000, Token(54)), virtI(x3I),Mneq(x0M,x0M),Mneq(x0M,x2M),insvIM(x3I,x2M)),
		Rule(55, FValue(0.010000, Token(55)), virtI(x3I),Mneq(x0M,x0M),Mneq(x0M,x2M),virtIM(x3I,x2M)),
		Rule(56, FValue(0.010000, Token(56)), virtI(x3I),Mneq(x0M,x2M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
		Rule(57, FValue(0.010000, Token(57)), virtI(x3I),Mneq(x0M,x2M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
		Rule(58, FValue(0.010000, Token(58)), virtI(x3I),Mneq(x0M,x0M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
		Rule(59, FValue(0.010143, Token(59)), virtI(x3I),Mneq(x0M,x0M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
		Rule(60, FValue(0.010000, Token(60)), virtI(x3I),Mneq(x1M,x2M),insvIM(x3I,x1M),insvIM(x3I,x2M)),
		Rule(61, FValue(0.010000, Token(61)), virtI(x3I),Mneq(x1M,x2M),insvIM(x3I,x1M),virtIM(x3I,x2M)),
		Rule(62, FValue(0.012458, Token(62)), virtI(x3I),Mneq(x1M,x2M),insvIM(x3I,x2M),virtIM(x3I,x1M)),
		Rule(63, FValue(0.010000, Token(63)), virtI(x3I),Mneq(x1M,x2M),virtIM(x3I,x1M),virtIM(x3I,x2M)),
		Rule(64, FValue(0.010000, Token(64)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x0M,x2M),Mneq(x2M,x0M)),
		Rule(65, FValue(0.010000, Token(65)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x0M,x0M),Mneq(x2M,x0M)),
		Rule(66, FValue(0.010000, Token(66)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x4I,x1M)),
		Rule(67, FValue(0.010000, Token(67)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x4I,x1M)),
		Rule(68, FValue(0.316642, Token(68)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x4I,x1M)),
		Rule(69, FValue(0.010000, Token(69)), virtI(x4I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x4I,x1M)),
		Rule(70, FValue(0.906114, Token(70)), virtI(x2I),Mneq(x0M,x0M),insvIM(x2I,x0M),virtIM(x2I,x3M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(71, FValue(0.000000, Token(71)), virtI(x2I),Mneq(x0M,x0M),insvIM(x2I,x3M),virtIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(72, FValue(0.000000, Token(72)), virtI(x2I),CICM(x3C,x2I,x4M),Mneq(x0M,x0M),insvIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(73, FValue(0.000000, Token(73)), virtI(x2I),CICM(x3C,x2I,x4M),Mneq(x0M,x0M),virtIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(74, FValue(0.000000, Token(74)), virtI(x2I),CICM(x3C,x2I,x0M),Mneq(x0M,x1M),insvIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(75, FValue(0.000000, Token(75)), virtI(x2I),CICM(x3C,x2I,x0M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(76, FValue(0.000000, Token(76)), virtI(x0I),CICM(x3C,x2I,x1M),insvIM(x0I,x1M),insvIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(77, FValue(0.000000, Token(77)), virtI(x0I),CICM(x3C,x2I,x1M),insvIM(x0I,x1M),virtIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(78, FValue(0.000000, Token(78)), virtI(x0I),CICM(x3C,x2I,x1M),insvIM(x2I,x1M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(79, FValue(0.000000, Token(79)), virtI(x0I),CICM(x3C,x2I,x1M),virtIM(x0I,x1M),virtIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(80, FValue(0.000000, Token(80)), polySite(x0I),insvIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(81, FValue(0.000000, Token(81)), polySite(x0I),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(82, FValue(0.000000, Token(82)), polySite(x1I),CICM(x0C,x1I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(83, FValue(0.000000, Token(83)), polySite(x0I),virtI(x0I)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(84, FValue(0.000000, Token(84)), polySite(x3I),insvIM(x0I,x1M),insvIM(x3I,x2M),virtIM(x0I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(85, FValue(0.000000, Token(85)), polySite(x3I),insvIM(x0I,x1M),virtIM(x0I,x2M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(86, FValue(0.000000, Token(86)), polySite(x3I),insvIM(x0I,x2M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(87, FValue(0.000000, Token(87)), polySite(x3I),insvIM(x0I,x2M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(88, FValue(0.000000, Token(88)), polySite(x3I),Mneq(x0M,x1M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(89, FValue(0.000000, Token(89)), polySite(x3I),Mneq(x0M,x1M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(90, FValue(0.000000, Token(90)), polySite(x3I),Mneq(x1M,x2M),insvIM(x0I,x1M),insvIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(91, FValue(0.000000, Token(91)), polySite(x3I),Mneq(x1M,x2M),insvIM(x0I,x1M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(92, FValue(0.000000, Token(92)), polySite(x3I),Mneq(x0M,x1M),Mneq(x1M,x2M),insvIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(93, FValue(0.000000, Token(93)), polySite(x3I),Mneq(x0M,x1M),Mneq(x1M,x2M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(94, FValue(0.000000, Token(94)), polySite(x3I),Mneq(x1M,x2M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(95, FValue(0.000000, Token(95)), polySite(x3I),Mneq(x1M,x2M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(96, FValue(0.000000, Token(96)), polySite(x3I),Mneq(x2M,x1M),insvIM(x0I,x1M),insvIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(97, FValue(0.000000, Token(97)), polySite(x3I),Mneq(x2M,x1M),insvIM(x0I,x1M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(98, FValue(0.000000, Token(98)), polySite(x3I),Mneq(x2M,x1M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(99, FValue(0.000000, Token(99)), polySite(x3I),Mneq(x2M,x1M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(100, FValue(0.000000, Token(100)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x2M,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(101, FValue(0.000000, Token(101)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(102, FValue(0.000000, Token(102)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x1M,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(103, FValue(0.000000, Token(103)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(104, FValue(0.000000, Token(104)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(105, FValue(0.000000, Token(105)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(106, FValue(0.000000, Token(106)), polySite(x2I),CICM(x3C,x2I,x4M),Mneq(x0M,x1M),insvIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(107, FValue(0.000000, Token(107)), polySite(x2I),CICM(x3C,x2I,x4M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(108, FValue(0.000000, Token(108)), polySite(x2I),Mneq(x0M,x1M),insvIM(x2I,x0M),virtI(x2I)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(109, FValue(0.000000, Token(109)), polySite(x2I),Mneq(x0M,x1M),virtI(x2I),virtIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(110, FValue(0.000000, Token(110)), polySite(x3I),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),insvIM(x3I,x4M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(111, FValue(0.000000, Token(111)), polySite(x3I),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),virtIM(x3I,x4M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(112, FValue(0.000000, Token(112)), polySite(x3I),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),virtI(x3I)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(113, FValue(0.000000, Token(113)), polySite(x0I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(114, FValue(0.000000, Token(114)), polySite(x0I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(115, FValue(0.000000, Token(115)), polySite(x0I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(116, FValue(0.000000, Token(116)), polySite(x0I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(117, FValue(0.000000, Token(117)), polySite(x0I),CICM(x3C,x2I,x4M),insvIM(x0I,x1M),insvIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(118, FValue(0.000000, Token(118)), polySite(x0I),CICM(x3C,x2I,x4M),insvIM(x0I,x1M),virtIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(119, FValue(0.000000, Token(119)), polySite(x0I),CICM(x3C,x2I,x4M),insvIM(x2I,x1M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(120, FValue(0.000000, Token(120)), polySite(x0I),CICM(x3C,x2I,x4M),virtIM(x0I,x1M),virtIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(121, FValue(0.000000, Token(121)), polySite(x0I),insvIM(x0I,x1M),insvIM(x2I,x1M),virtI(x2I)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(122, FValue(0.000000, Token(122)), polySite(x0I),insvIM(x0I,x1M),insvIM(x2I,x1M),polySite(x2I)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(123, FValue(0.000000, Token(123)), polySite(x0I),insvIM(x0I,x1M),virtI(x2I),virtIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(124, FValue(0.000000, Token(124)), polySite(x0I),insvIM(x0I,x1M),polySite(x2I),virtIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(125, FValue(0.000000, Token(125)), polySite(x0I),insvIM(x2I,x1M),virtI(x2I),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(126, FValue(0.000000, Token(126)), polySite(x0I),insvIM(x2I,x1M),polySite(x2I),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(127, FValue(0.000000, Token(127)), polySite(x0I),virtI(x2I),virtIM(x0I,x1M),virtIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(128, FValue(0.000000, Token(128)), polySite(x0I),polySite(x2I),virtIM(x0I,x1M),virtIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(129, FValue(0.000000, Token(129)), polySite(x4I),CICM(x2C,x0I,x3M),insvIM(x0I,x1M),insvIM(x4I,x3M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(130, FValue(0.000000, Token(130)), polySite(x4I),CICM(x2C,x0I,x3M),insvIM(x0I,x1M),virtIM(x4I,x3M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(131, FValue(0.000000, Token(131)), polySite(x4I),CICM(x2C,x0I,x3M),insvIM(x4I,x3M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(132, FValue(0.000000, Token(132)), polySite(x4I),CICM(x2C,x0I,x3M),virtIM(x0I,x1M),virtIM(x4I,x3M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(133, FValue(0.000000, Token(133)), polySite(x3I),Mneq(x0M,x0M),Mneq(x0M,x2M),insvIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(134, FValue(0.000000, Token(134)), polySite(x3I),Mneq(x0M,x0M),Mneq(x0M,x2M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(135, FValue(0.000000, Token(135)), polySite(x3I),Mneq(x0M,x2M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(136, FValue(0.000000, Token(136)), polySite(x3I),Mneq(x0M,x2M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(137, FValue(0.000000, Token(137)), polySite(x3I),Mneq(x0M,x0M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(138, FValue(0.000000, Token(138)), polySite(x3I),Mneq(x0M,x0M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(139, FValue(0.000000, Token(139)), polySite(x3I),Mneq(x1M,x2M),insvIM(x3I,x1M),insvIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(140, FValue(0.000000, Token(140)), polySite(x3I),Mneq(x1M,x2M),insvIM(x3I,x1M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(141, FValue(0.000000, Token(141)), polySite(x3I),Mneq(x1M,x2M),insvIM(x3I,x2M),virtIM(x3I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(142, FValue(0.000000, Token(142)), polySite(x3I),Mneq(x1M,x2M),virtIM(x3I,x1M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(143, FValue(0.000000, Token(143)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x0M,x2M),Mneq(x2M,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(144, FValue(0.000000, Token(144)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x0M,x0M),Mneq(x2M,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(145, FValue(0.000000, Token(145)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x4I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(146, FValue(0.000000, Token(146)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x4I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(147, FValue(0.000000, Token(147)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x4I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(148, FValue(0.000000, Token(148)), polySite(x4I),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x4I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(149, FValue(0.000000, Token(149)), polySite(x2I),Mneq(x0M,x0M),insvIM(x2I,x0M),virtIM(x2I,x3M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(150, FValue(0.000000, Token(150)), polySite(x2I),Mneq(x0M,x0M),insvIM(x2I,x3M),virtIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(151, FValue(0.000000, Token(151)), polySite(x2I),CICM(x3C,x2I,x4M),Mneq(x0M,x0M),insvIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(152, FValue(0.000000, Token(152)), polySite(x2I),CICM(x3C,x2I,x4M),Mneq(x0M,x0M),virtIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(153, FValue(0.000000, Token(153)), polySite(x2I),CICM(x3C,x2I,x0M),Mneq(x0M,x1M),insvIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(154, FValue(0.000000, Token(154)), polySite(x2I),CICM(x3C,x2I,x0M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(155, FValue(0.000000, Token(155)), polySite(x0I),CICM(x3C,x2I,x1M),insvIM(x0I,x1M),insvIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(156, FValue(0.000000, Token(156)), polySite(x0I),CICM(x3C,x2I,x1M),insvIM(x0I,x1M),virtIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(157, FValue(0.000000, Token(157)), polySite(x0I),CICM(x3C,x2I,x1M),insvIM(x2I,x1M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(158, FValue(0.000000, Token(158)), polySite(x0I),CICM(x3C,x2I,x1M),virtIM(x0I,x1M),virtIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(159, FValue(0.000000, Token(159)), insvIM(x0I,x1M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(160, FValue(0.000000, Token(160)), insvIM(x1I,x2M),CICM(x0C,x1I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(161, FValue(0.000000, Token(161)), insvIM(x3I,x1M),insvIM(x0I,x1M),insvIM(x0I,x2M),insvIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(162, FValue(0.000000, Token(162)), insvIM(x3I,x1M),insvIM(x0I,x1M),insvIM(x0I,x2M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(163, FValue(0.000000, Token(163)), insvIM(x3I,x1M),insvIM(x0I,x1M),insvIM(x3I,x2M),virtIM(x0I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(164, FValue(0.000000, Token(164)), insvIM(x3I,x1M),insvIM(x0I,x1M),virtIM(x0I,x2M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(165, FValue(0.000000, Token(165)), insvIM(x3I,x1M),Mneq(x0M,x1M),Mneq(x0M,x2M),insvIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(166, FValue(0.000000, Token(166)), insvIM(x3I,x1M),Mneq(x0M,x1M),Mneq(x0M,x2M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(167, FValue(0.000000, Token(167)), insvIM(x3I,x1M),insvIM(x0I,x2M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(168, FValue(0.000000, Token(168)), insvIM(x3I,x1M),insvIM(x0I,x2M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(169, FValue(0.000000, Token(169)), insvIM(x3I,x1M),insvIM(x3I,x2M),virtIM(x0I,x1M),virtIM(x0I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(170, FValue(0.000000, Token(170)), insvIM(x3I,x1M),virtIM(x0I,x1M),virtIM(x0I,x2M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(171, FValue(0.000000, Token(171)), insvIM(x3I,x0M),Mneq(x0M,x1M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(172, FValue(0.000000, Token(172)), insvIM(x3I,x0M),Mneq(x0M,x1M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(173, FValue(0.000000, Token(173)), insvIM(x3I,x1M),Mneq(x0M,x1M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(174, FValue(0.000000, Token(174)), insvIM(x3I,x1M),Mneq(x0M,x1M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(175, FValue(0.000000, Token(175)), insvIM(x3I,x0M),Mneq(x0M,x1M),Mneq(x1M,x2M),insvIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(176, FValue(0.000000, Token(176)), insvIM(x3I,x0M),Mneq(x0M,x1M),Mneq(x1M,x2M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(177, FValue(0.000000, Token(177)), insvIM(x3I,x1M),Mneq(x1M,x2M),insvIM(x0I,x1M),insvIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(178, FValue(0.000000, Token(178)), insvIM(x3I,x1M),Mneq(x1M,x2M),insvIM(x0I,x1M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(179, FValue(0.000000, Token(179)), insvIM(x3I,x1M),Mneq(x0M,x1M),Mneq(x1M,x2M),insvIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(180, FValue(0.000000, Token(180)), insvIM(x3I,x1M),Mneq(x0M,x1M),Mneq(x1M,x2M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(181, FValue(0.000000, Token(181)), insvIM(x3I,x1M),Mneq(x1M,x2M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(182, FValue(0.000000, Token(182)), insvIM(x3I,x1M),Mneq(x1M,x2M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(183, FValue(0.000000, Token(183)), insvIM(x3I,x0M),Mneq(x0M,x1M),Mneq(x2M,x1M),insvIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(184, FValue(0.000000, Token(184)), insvIM(x3I,x0M),Mneq(x0M,x1M),Mneq(x2M,x1M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(185, FValue(0.000000, Token(185)), insvIM(x3I,x1M),Mneq(x2M,x1M),insvIM(x0I,x1M),insvIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(186, FValue(0.000000, Token(186)), insvIM(x3I,x1M),Mneq(x2M,x1M),insvIM(x0I,x1M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(187, FValue(0.000000, Token(187)), insvIM(x3I,x1M),Mneq(x2M,x1M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(188, FValue(0.000000, Token(188)), insvIM(x3I,x1M),Mneq(x2M,x1M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(189, FValue(0.000000, Token(189)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),insvIM(x0I,x1M),insvIM(x0I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(190, FValue(0.000000, Token(190)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),insvIM(x0I,x1M),virtIM(x0I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(191, FValue(0.000000, Token(191)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x0M,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(192, FValue(0.000000, Token(192)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),insvIM(x0I,x2M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(193, FValue(0.000000, Token(193)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),virtIM(x0I,x1M),virtIM(x0I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(194, FValue(0.000000, Token(194)), insvIM(x4I,x0M),CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x2M,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(195, FValue(0.000000, Token(195)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x2M,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(196, FValue(0.000000, Token(196)), insvIM(x4I,x0M),CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x1M,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(197, FValue(0.000000, Token(197)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(198, FValue(0.000000, Token(198)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x1M,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(199, FValue(0.000000, Token(199)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(200, FValue(0.000000, Token(200)), insvIM(x4I,x0M),CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x2M,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(201, FValue(0.000000, Token(201)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(202, FValue(0.000000, Token(202)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(203, FValue(0.000000, Token(203)), insvIM(x2I,x0M),Mneq(x0M,x1M),insvIM(x2I,x3M),virtIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(204, FValue(0.000000, Token(204)), insvIM(x2I,x1M),insvIM(x0I,x1M),insvIM(x2I,x3M),virtIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(205, FValue(0.000000, Token(205)), insvIM(x2I,x1M),Mneq(x0M,x1M),insvIM(x2I,x3M),virtIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(206, FValue(0.000000, Token(206)), insvIM(x2I,x0M),CICM(x3C,x2I,x4M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(207, FValue(0.000000, Token(207)), insvIM(x2I,x1M),CICM(x3C,x2I,x4M),Mneq(x0M,x1M),insvIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(208, FValue(0.000000, Token(208)), insvIM(x2I,x1M),CICM(x3C,x2I,x4M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(209, FValue(0.000000, Token(209)), insvIM(x2I,x1M),CICM(x3C,x2I,x4M),insvIM(x0I,x1M),virtIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(210, FValue(0.000000, Token(210)), insvIM(x2I,x1M),CICM(x3C,x2I,x4M),Mneq(x0M,x1M),virtIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(211, FValue(0.000000, Token(211)), insvIM(x2I,x0M),Mneq(x0M,x1M),virtI(x2I),virtIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(212, FValue(0.000000, Token(212)), insvIM(x2I,x0M),Mneq(x0M,x1M),polySite(x2I),virtIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(213, FValue(0.000000, Token(213)), insvIM(x2I,x1M),Mneq(x0M,x1M),insvIM(x2I,x0M),virtI(x2I)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(214, FValue(0.000000, Token(214)), insvIM(x2I,x1M),Mneq(x0M,x1M),insvIM(x2I,x0M),polySite(x2I)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(215, FValue(0.000000, Token(215)), insvIM(x2I,x1M),Mneq(x0M,x1M),virtI(x2I),virtIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(216, FValue(0.000000, Token(216)), insvIM(x2I,x1M),Mneq(x0M,x1M),polySite(x2I),virtIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(217, FValue(0.000000, Token(217)), insvIM(x2I,x1M),insvIM(x0I,x1M),virtI(x2I),virtIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(218, FValue(0.000000, Token(218)), insvIM(x2I,x1M),insvIM(x0I,x1M),polySite(x2I),virtIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(219, FValue(0.000000, Token(219)), insvIM(x2I,x1M),Mneq(x0M,x1M),virtI(x2I),virtIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(220, FValue(0.000000, Token(220)), insvIM(x2I,x1M),Mneq(x0M,x1M),polySite(x2I),virtIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(221, FValue(0.000000, Token(221)), insvIM(x3I,x0M),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),insvIM(x3I,x4M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(222, FValue(0.000000, Token(222)), insvIM(x3I,x0M),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),virtIM(x3I,x4M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(223, FValue(0.000000, Token(223)), insvIM(x3I,x1M),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),insvIM(x3I,x4M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(224, FValue(0.000000, Token(224)), insvIM(x3I,x1M),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),virtIM(x3I,x4M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(225, FValue(0.000000, Token(225)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),insvIM(x0I,x1M),insvIM(x3I,x4M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(226, FValue(0.000000, Token(226)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),insvIM(x0I,x1M),virtIM(x3I,x4M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(227, FValue(0.000000, Token(227)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),Mneq(x0M,x1M),insvIM(x3I,x4M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(228, FValue(0.000000, Token(228)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),Mneq(x0M,x1M),virtIM(x3I,x4M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(229, FValue(0.000000, Token(229)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),insvIM(x3I,x4M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(230, FValue(0.000000, Token(230)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),virtIM(x0I,x1M),virtIM(x3I,x4M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(231, FValue(0.000000, Token(231)), insvIM(x4I,x1M),CICM(x2C,x3I,x0M),CICM(x2C,x4I,x5M),Mneq(x0M,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(232, FValue(0.000000, Token(232)), insvIM(x3I,x0M),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),virtI(x3I)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(233, FValue(0.000000, Token(233)), insvIM(x3I,x0M),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),polySite(x3I)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(234, FValue(0.000000, Token(234)), insvIM(x3I,x1M),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),virtI(x3I)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(235, FValue(0.000000, Token(235)), insvIM(x3I,x1M),CICM(x2C,x3I,x0M),Mneq(x0M,x1M),polySite(x3I)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(236, FValue(0.000000, Token(236)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),insvIM(x0I,x1M),virtI(x3I)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(237, FValue(0.000000, Token(237)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),insvIM(x0I,x1M),polySite(x3I)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(238, FValue(0.000000, Token(238)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),Mneq(x0M,x1M),virtI(x3I)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(239, FValue(0.000000, Token(239)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),Mneq(x0M,x1M),polySite(x3I)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(240, FValue(0.000000, Token(240)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),virtI(x3I),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(241, FValue(0.000000, Token(241)), insvIM(x3I,x1M),CICM(x2C,x3I,x1M),polySite(x3I),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(242, FValue(0.000000, Token(242)), insvIM(x0I,x2M),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(243, FValue(0.000000, Token(243)), insvIM(x0I,x2M),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(244, FValue(0.000000, Token(244)), insvIM(x0I,x2M),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(245, FValue(0.000000, Token(245)), insvIM(x0I,x2M),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(246, FValue(0.000000, Token(246)), insvIM(x0I,x2M),CICM(x3C,x4I,x2M),insvIM(x0I,x1M),virtIM(x0I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(247, FValue(0.000000, Token(247)), insvIM(x0I,x4M),CICM(x3C,x2I,x4M),insvIM(x0I,x1M),insvIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(248, FValue(0.000000, Token(248)), insvIM(x0I,x4M),CICM(x3C,x2I,x4M),insvIM(x0I,x1M),virtIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(249, FValue(0.000000, Token(249)), insvIM(x0I,x4M),CICM(x3C,x2I,x4M),insvIM(x2I,x1M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(250, FValue(0.000000, Token(250)), insvIM(x0I,x4M),CICM(x3C,x2I,x4M),virtIM(x0I,x1M),virtIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(251, FValue(0.000000, Token(251)), insvIM(x4I,x1M),CICM(x2C,x0I,x3M),insvIM(x0I,x1M),insvIM(x4I,x3M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(252, FValue(0.000000, Token(252)), insvIM(x4I,x1M),CICM(x2C,x0I,x3M),insvIM(x0I,x1M),virtIM(x4I,x3M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(253, FValue(0.000000, Token(253)), insvIM(x4I,x1M),CICM(x2C,x0I,x3M),insvIM(x4I,x3M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(254, FValue(0.000000, Token(254)), insvIM(x4I,x1M),CICM(x2C,x0I,x3M),virtIM(x0I,x1M),virtIM(x4I,x3M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(255, FValue(0.000000, Token(255)), insvIM(x4I,x1M),CICM(x2C,x0I,x3M),CICM(x2C,x4I,x5M),insvIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(256, FValue(0.000000, Token(256)), insvIM(x4I,x1M),CICM(x2C,x0I,x3M),CICM(x2C,x4I,x5M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(257, FValue(0.000000, Token(257)), insvIM(x5I,x1M),CICM(x2C,x0I,x3M),CICM(x4C,x5I,x3M),insvIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(258, FValue(0.000000, Token(258)), insvIM(x5I,x1M),CICM(x2C,x0I,x3M),CICM(x4C,x5I,x3M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(259, FValue(0.000000, Token(259)), insvIM(x0I,x5M),CICM(x2C,x3I,x1M),CICM(x4C,x3I,x5M),insvIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(260, FValue(0.000000, Token(260)), insvIM(x0I,x5M),CICM(x2C,x3I,x1M),CICM(x4C,x3I,x5M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(261, FValue(0.000000, Token(261)), insvIM(x1I,x3M),CICM(x0C,x1I,x2M),CICM(x4C,x5I,x3M),Mneq(x2M,x3M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(262, FValue(0.000000, Token(262)), insvIM(x3I,x2M),CICM(x0C,x1I,x2M),virtI(x3I),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(263, FValue(0.000000, Token(263)), insvIM(x3I,x2M),CICM(x0C,x1I,x2M),polySite(x3I),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(264, FValue(0.000000, Token(264)), insvIM(x3I,x0M),Mneq(x0M,x2M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(265, FValue(0.000000, Token(265)), insvIM(x3I,x0M),Mneq(x0M,x2M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(266, FValue(0.000000, Token(266)), insvIM(x3I,x1M),Mneq(x1M,x2M),insvIM(x3I,x2M),virtIM(x3I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(267, FValue(0.000000, Token(267)), insvIM(x3I,x1M),Mneq(x1M,x2M),virtIM(x3I,x1M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(268, FValue(0.000000, Token(268)), insvIM(x3I,x1M),Mneq(x2M,x1M),insvIM(x3I,x2M),virtIM(x3I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(269, FValue(0.000000, Token(269)), insvIM(x3I,x1M),Mneq(x2M,x1M),virtIM(x3I,x1M),virtIM(x3I,x2M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(270, FValue(0.000000, Token(270)), insvIM(x4I,x0M),CICM(x3C,x4I,x2M),Mneq(x0M,x2M),Mneq(x2M,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(271, FValue(0.000000, Token(271)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x4I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(272, FValue(0.000000, Token(272)), insvIM(x4I,x1M),CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x4I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(273, FValue(0.000000, Token(273)), insvIM(x2I,x1M),CICM(x3C,x2I,x0M),Mneq(x0M,x1M),insvIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(274, FValue(0.000000, Token(274)), insvIM(x2I,x1M),CICM(x3C,x2I,x0M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(275, FValue(0.000000, Token(275)), insvIM(x2I,x1M),CICM(x3C,x2I,x1M),Mneq(x0M,x1M),insvIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(276, FValue(0.000000, Token(276)), insvIM(x2I,x1M),CICM(x3C,x2I,x1M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(277, FValue(0.000000, Token(277)), insvIM(x2I,x0M),CICM(x3C,x2I,x0M),Mneq(x0M,x1M),insvIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(278, FValue(0.000000, Token(278)), insvIM(x2I,x0M),CICM(x3C,x2I,x0M),Mneq(x0M,x1M),virtIM(x2I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(279, FValue(0.000000, Token(279)), insvIM(x4I,x1M),CICM(x2C,x3I,x0M),CICM(x2C,x4I,x1M),Mneq(x0M,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(280, FValue(0.000000, Token(280)), insvIM(x3I,x1M),CICM(x2C,x3I,x0M),CICM(x4C,x3I,x1M),Mneq(x0M,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(281, FValue(0.000000, Token(281)), insvIM(x4I,x1M),CICM(x2C,x3I,x1M),CICM(x2C,x4I,x5M),insvIM(x3I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(282, FValue(0.000000, Token(282)), insvIM(x4I,x1M),CICM(x2C,x3I,x1M),CICM(x2C,x4I,x5M),virtIM(x3I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(283, FValue(0.000000, Token(283)), insvIM(x4I,x1M),CICM(x2C,x3I,x1M),CICM(x2C,x4I,x5M),Mneq(x5M,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(284, FValue(0.000000, Token(284)), insvIM(x4I,x1M),CICM(x2C,x0I,x5M),CICM(x2C,x4I,x5M),insvIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(285, FValue(0.000000, Token(285)), insvIM(x4I,x1M),CICM(x2C,x0I,x5M),CICM(x2C,x4I,x5M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(286, FValue(0.000000, Token(286)), insvIM(x4I,x1M),CICM(x2C,x0I,x3M),CICM(x2C,x4I,x1M),insvIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(287, FValue(0.000000, Token(287)), insvIM(x4I,x1M),CICM(x2C,x0I,x3M),CICM(x2C,x4I,x1M),virtIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(288, FValue(0.000000, Token(288)), insvIM(x0I,x5M),CICM(x2C,x4I,x1M),CICM(x2C,x4I,x5M),insvIM(x0I,x1M)),
// WARN: not enough rule weights. maybe because of rule-cutoff
		Rule(289, FValue(0.000000, Token(289)), insvIM(x0I,x5M),CICM(x2C,x4I,x1M),CICM(x2C,x4I,x5M),virtIM(x0I,x1M)),
  )

  override val expected: Set[Any] = Set(2, 139, 160)
  override val maxVarCount: Int = 3
  val usefulTokens= Set(2)
  val soup =
    soup_pre.map(r => Rule(r.name, FValue(1.0, r.coeff.prov), r.head, r.body)).
    filter(r => usefulTokens.contains(r.name.asInstanceOf[Int]))
}
