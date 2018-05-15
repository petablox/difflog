package qd
package learner

import org.scalatest.{FunSuite, Ignore}

@Ignore
class Polysite extends FunSuite {
  val ISet = Range(0, 17).map(i => Atom(i)).toSet
  val I = Domain("I", ISet)

  val CSet = Range(0, 1).map(i => Atom(i)).toSet
  val C = Domain("C", CSet)

  val MSet = Range(0, 12).map(i => Atom(i)).toSet
  val M = Domain("M", MSet)

  val CICM : Relation = Relation("CICM", C, I, M)
  val virtIM = Relation("virtIM", I, M)
  val Mneq = Relation("Mneq", M, M)

  val insvIM = Relation("insvIM", I, M)
  val virtI = Relation("virtI", I)
  val polySite = Relation("polySite", I)

  val CICMTuples = Set((0,0,10),(0,8,5),(0,4,8),(0,12,7),(0,2,1),(0,10,6),(0,6,7),(0,14,4),
    (0,14,6),(0,1,1),(0,9,7),(0,5,1),(0,13,4),(0,3,4),(0,3,2),(0,3,6),
    (0,11,3),(0,7,2),(0,15,7)).map{case (a,b,c) => DTuple(Atom(a), Atom(b), Atom(c))}
  val virtIMTuples = Set((10,2),(14,2),(13,2),(3,2),(7,2),(16,11)).map{case(a,b) => DTuple(Atom(a), Atom(b))}
  val pairs_list = for (i <- 0 to 11; j <- 0 to 11) yield (i,j)
  val filtered : IndexedSeq[(Int, Int)] = pairs_list.filter { case (a,b) => a != b }
  val MneqTuples : Set[DTuple] = Set(filtered : _*).map{ case (a) => DTuple(Atom(a._1), Atom(a._2)) }

  val insvIMTuples = Set((0,10),(8,5),(4,8),(12,7),(2,1),(10,6),(6,7),(14,4),(14,6),(1,1),(9,7),
    (5,1),(13,4),(3,4),(3,2),(3,6),(11,3),(7,2),(15,7)).map{case (a,b) => DTuple(Atom(a), Atom(b))}
  val virtITuples = Set((16),(10),(14),(13),(3),(7)).map{ case (x) => DTuple(Atom(x)) }
  val polysiteTuples = Set(14, 3).map{ case (x) => DTuple(Atom(x)) }

  val edb : Config = Config(
    CICM -> (Instance(CICM) ++ CICMTuples.map(t => t -> One).toMap),
    virtIM -> (Instance(virtIM) ++ virtIMTuples.map(t => t -> One).toMap),
    Mneq -> (Instance(Mneq) ++ MneqTuples.map(t => t -> One).toMap),
  )

  val refOut : Config = Config (
    insvIM -> (Instance(insvIM) ++ insvIMTuples.map(t => t -> One).toMap),
    virtI -> (Instance(virtI) ++ virtITuples.map(t => t -> One).toMap),
    polySite -> (Instance(polySite) ++ polysiteTuples.map(t => t -> One).toMap)
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

  val soup : Set[Rule] = Set(
    Rule(1	,Value(0.5, Token(1	)),virtI(x0I), insvIM(x0I,x1M)),
    Rule(2	,Value(0.5, Token(2	)),virtI(x0I), virtIM(x0I,x1M)),
    Rule(3	,Value(0.5, Token(3	)),virtI(x1I), CICM(x0C,x1I,x2M)),
    Rule(4	,Value(0.5, Token(4	)),virtI(x0I), polySite(x0I)),
    Rule(5	,Value(0.5, Token(5	)),virtI(x3I), insvIM(x0I,x1M),insvIM(x3I,x2M),virtIM(x0I,x2M)),
    Rule(6	,Value(0.5, Token(6	)),virtI(x3I), insvIM(x0I,x1M),virtIM(x0I,x2M),virtIM(x3I,x2M)),
    Rule(7	,Value(0.5, Token(7	)),virtI(x3I), insvIM(x0I,x2M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
    Rule(8	,Value(0.5, Token(8	)),virtI(x3I), insvIM(x0I,x2M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
    Rule(9	,Value(0.5, Token(9	)),virtI(x3I), Mneq(x0M,x1M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
    Rule(10	,Value(0.5, Token(10	)),virtI(x3I), Mneq(x0M,x1M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
    Rule(11	,Value(0.5, Token(11	)),virtI(x3I), Mneq(x1M,x2M),insvIM(x0I,x1M),insvIM(x3I,x2M)),
    Rule(12	,Value(0.5, Token(12	)),virtI(x3I), Mneq(x1M,x2M),insvIM(x0I,x1M),virtIM(x3I,x2M)),
    Rule(13	,Value(0.5, Token(13	)),virtI(x3I), Mneq(x0M,x1M),Mneq(x1M,x2M),insvIM(x3I,x2M)),
    Rule(14	,Value(0.5, Token(14	)),virtI(x3I), Mneq(x0M,x1M),Mneq(x1M,x2M),virtIM(x3I,x2M)),
    Rule(15	,Value(0.5, Token(15	)),virtI(x3I), Mneq(x1M,x2M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
    Rule(16	,Value(0.5, Token(16	)),virtI(x3I), Mneq(x1M,x2M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
    Rule(17	,Value(0.5, Token(17	)),virtI(x3I), Mneq(x2M,x1M),insvIM(x0I,x1M),insvIM(x3I,x2M)),
    Rule(18	,Value(0.5, Token(18	)),virtI(x3I), Mneq(x2M,x1M),insvIM(x0I,x1M),virtIM(x3I,x2M)),
    Rule(19	,Value(0.5, Token(19	)),virtI(x3I), Mneq(x2M,x1M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
    Rule(20	,Value(0.5, Token(20	)),virtI(x3I), Mneq(x2M,x1M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
    Rule(21	,Value(0.5, Token(21	)),virtI(x4I), CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x2M,x0M)),
    Rule(22	,Value(0.5, Token(22	)),virtI(x4I), CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x0I,x1M)),
    Rule(23	,Value(0.5, Token(23	)),virtI(x4I), CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x1M,x2M)),
    Rule(24	,Value(0.5, Token(24	)),virtI(x4I), CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x0I,x1M)),
    Rule(25	,Value(0.5, Token(25	)),virtI(x4I), CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x0I,x1M)),
    Rule(26	,Value(0.5, Token(26	)),virtI(x4I), CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x0I,x1M)),
    Rule(27	,Value(0.5, Token(27	)),virtI(x2I), CICM(x3C,x2I,x4M),Mneq(x0M,x1M),insvIM(x2I,x0M)),
    Rule(28	,Value(0.5, Token(28	)),virtI(x2I), CICM(x3C,x2I,x4M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
    Rule(29	,Value(0.5, Token(29	)),virtI(x2I), Mneq(x0M,x1M),insvIM(x2I,x0M),polySite(x2I)),
    Rule(30	,Value(0.5, Token(30	)),virtI(x2I), Mneq(x0M,x1M),polySite(x2I),virtIM(x2I,x0M)),
    Rule(31	,Value(0.5, Token(31	)),virtI(x3I), CICM(x2C,x3I,x0M),Mneq(x0M,x1M),insvIM(x3I,x4M)),
    Rule(32	,Value(0.5, Token(32	)),virtI(x3I), CICM(x2C,x3I,x0M),Mneq(x0M,x1M),virtIM(x3I,x4M)),
    Rule(33	,Value(0.5, Token(33	)),virtI(x3I), CICM(x2C,x3I,x0M),Mneq(x0M,x1M),polySite(x3I)),
    Rule(34	,Value(0.5, Token(34	)),virtI(x0I), CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x0I,x1M)),
    Rule(35	,Value(0.5, Token(35	)),virtI(x0I), CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x0I,x1M)),
    Rule(36	,Value(0.5, Token(36	)),virtI(x0I), CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x0I,x1M)),
    Rule(37	,Value(0.5, Token(37	)),virtI(x0I), CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x0I,x1M)),
    Rule(38	,Value(0.5, Token(38	)),virtI(x0I), CICM(x3C,x2I,x4M),insvIM(x0I,x1M),insvIM(x2I,x1M)),
    Rule(39	,Value(0.5, Token(39	)),virtI(x0I), CICM(x3C,x2I,x4M),insvIM(x0I,x1M),virtIM(x2I,x1M)),
    Rule(40	,Value(0.5, Token(40	)),virtI(x0I), CICM(x3C,x2I,x4M),insvIM(x2I,x1M),virtIM(x0I,x1M)),
    Rule(41	,Value(0.5, Token(41	)),virtI(x0I), CICM(x3C,x2I,x4M),virtIM(x0I,x1M),virtIM(x2I,x1M)),
    Rule(42	,Value(0.5, Token(42	)),virtI(x0I), insvIM(x0I,x1M),insvIM(x2I,x1M),virtI(x2I)),
    Rule(43	,Value(0.5, Token(43	)),virtI(x0I), insvIM(x0I,x1M),insvIM(x2I,x1M),polySite(x2I)),
    Rule(44	,Value(0.5, Token(44	)),virtI(x0I), insvIM(x0I,x1M),virtI(x2I),virtIM(x2I,x1M)),
    Rule(45	,Value(0.5, Token(45	)),virtI(x0I), insvIM(x0I,x1M),polySite(x2I),virtIM(x2I,x1M)),
    Rule(46	,Value(0.5, Token(46	)),virtI(x0I), insvIM(x2I,x1M),virtI(x2I),virtIM(x0I,x1M)),
    Rule(47	,Value(0.5, Token(47	)),virtI(x0I), insvIM(x2I,x1M),polySite(x2I),virtIM(x0I,x1M)),
    Rule(48	,Value(0.5, Token(48	)),virtI(x0I), virtI(x2I),virtIM(x0I,x1M),virtIM(x2I,x1M)),
    Rule(49	,Value(0.5, Token(49	)),virtI(x0I), polySite(x2I),virtIM(x0I,x1M),virtIM(x2I,x1M)),
    Rule(50	,Value(0.5, Token(50	)),virtI(x4I), CICM(x2C,x0I,x3M),insvIM(x0I,x1M),insvIM(x4I,x3M)),
    Rule(51	,Value(0.5, Token(51	)),virtI(x4I), CICM(x2C,x0I,x3M),insvIM(x0I,x1M),virtIM(x4I,x3M)),
    Rule(52	,Value(0.5, Token(52	)),virtI(x4I), CICM(x2C,x0I,x3M),insvIM(x4I,x3M),virtIM(x0I,x1M)),
    Rule(53	,Value(0.5, Token(53	)),virtI(x4I), CICM(x2C,x0I,x3M),virtIM(x0I,x1M),virtIM(x4I,x3M)),
    Rule(54	,Value(0.5, Token(54	)),virtI(x3I), Mneq(x0M,x0M),Mneq(x0M,x2M),insvIM(x3I,x2M)),
    Rule(55	,Value(0.5, Token(55	)),virtI(x3I), Mneq(x0M,x0M),Mneq(x0M,x2M),virtIM(x3I,x2M)),
    Rule(56	,Value(0.5, Token(56	)),virtI(x3I), Mneq(x0M,x2M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
    Rule(57	,Value(0.5, Token(57	)),virtI(x3I), Mneq(x0M,x2M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
    Rule(58	,Value(0.5, Token(58	)),virtI(x3I), Mneq(x0M,x0M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
    Rule(59	,Value(0.5, Token(59	)),virtI(x3I), Mneq(x0M,x0M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
    Rule(60	,Value(0.5, Token(60	)),virtI(x3I), Mneq(x1M,x2M),insvIM(x3I,x1M),insvIM(x3I,x2M)),
    Rule(61	,Value(0.5, Token(61	)),virtI(x3I), Mneq(x1M,x2M),insvIM(x3I,x1M),virtIM(x3I,x2M)),
    Rule(62	,Value(0.5, Token(62	)),virtI(x3I), Mneq(x1M,x2M),insvIM(x3I,x2M),virtIM(x3I,x1M)),
    Rule(63	,Value(0.5, Token(63	)),virtI(x3I), Mneq(x1M,x2M),virtIM(x3I,x1M),virtIM(x3I,x2M)),
    Rule(64	,Value(0.5, Token(64	)),virtI(x4I), CICM(x3C,x4I,x2M),Mneq(x0M,x2M),Mneq(x2M,x0M)),
    Rule(65	,Value(0.5, Token(65	)),virtI(x4I), CICM(x3C,x4I,x2M),Mneq(x0M,x0M),Mneq(x2M,x0M)),
    Rule(66	,Value(0.5, Token(66	)),virtI(x4I), CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x4I,x1M)),
    Rule(67	,Value(0.5, Token(67	)),virtI(x4I), CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x4I,x1M)),
    Rule(68	,Value(0.5, Token(68	)),virtI(x4I), CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x4I,x1M)),
    Rule(69	,Value(0.5, Token(69	)),virtI(x4I), CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x4I,x1M)),
    Rule(70	,Value(0.5, Token(70	)),virtI(x2I), Mneq(x0M,x0M),insvIM(x2I,x0M),virtIM(x2I,x3M)),
    Rule(71	,Value(0.5, Token(71	)),virtI(x2I), Mneq(x0M,x0M),insvIM(x2I,x3M),virtIM(x2I,x0M)),
    Rule(72	,Value(0.5, Token(72	)),virtI(x2I), CICM(x3C,x2I,x4M),Mneq(x0M,x0M),insvIM(x2I,x0M)),
    Rule(73	,Value(0.5, Token(73	)),virtI(x2I), CICM(x3C,x2I,x4M),Mneq(x0M,x0M),virtIM(x2I,x0M)),
    Rule(74	,Value(0.5, Token(74	)),virtI(x2I), CICM(x3C,x2I,x0M),Mneq(x0M,x1M),insvIM(x2I,x0M)),
    Rule(75	,Value(0.5, Token(75	)),virtI(x2I), CICM(x3C,x2I,x0M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
    Rule(76	,Value(0.5, Token(76	)),virtI(x0I), CICM(x3C,x2I,x1M),insvIM(x0I,x1M),insvIM(x2I,x1M)),
    Rule(77	,Value(0.5, Token(77	)),virtI(x0I), CICM(x3C,x2I,x1M),insvIM(x0I,x1M),virtIM(x2I,x1M)),
    Rule(78	,Value(0.5, Token(78	)),virtI(x0I), CICM(x3C,x2I,x1M),insvIM(x2I,x1M),virtIM(x0I,x1M)),
    Rule(79	,Value(0.5, Token(79	)),virtI(x0I), CICM(x3C,x2I,x1M),virtIM(x0I,x1M),virtIM(x2I,x1M)),
    Rule(80	,Value(0.5, Token(80	)),polySite(x0I), insvIM(x0I,x1M)),
    Rule(81	,Value(0.5, Token(81	)),polySite(x0I), virtIM(x0I,x1M)),
    Rule(82	,Value(0.5, Token(82	)),polySite(x1I), CICM(x0C,x1I,x2M)),
    Rule(83	,Value(0.5, Token(83	)),polySite(x0I), virtI(x0I)),
    Rule(84	,Value(0.5, Token(84	)),polySite(x3I), insvIM(x0I,x1M),insvIM(x3I,x2M),virtIM(x0I,x2M)),
    Rule(85	,Value(0.5, Token(85	)),polySite(x3I), insvIM(x0I,x1M),virtIM(x0I,x2M),virtIM(x3I,x2M)),
    Rule(86	,Value(0.5, Token(86	)),polySite(x3I), insvIM(x0I,x2M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
    Rule(87	,Value(0.5, Token(87	)),polySite(x3I), insvIM(x0I,x2M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
    Rule(88	,Value(0.5, Token(88	)),polySite(x3I), Mneq(x0M,x1M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
    Rule(89	,Value(0.5, Token(89	)),polySite(x3I), Mneq(x0M,x1M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
    Rule(90	,Value(0.5, Token(90	)),polySite(x3I), Mneq(x1M,x2M),insvIM(x0I,x1M),insvIM(x3I,x2M)),
    Rule(91	,Value(0.5, Token(91	)),polySite(x3I), Mneq(x1M,x2M),insvIM(x0I,x1M),virtIM(x3I,x2M)),
    Rule(92	,Value(0.5, Token(92	)),polySite(x3I), Mneq(x0M,x1M),Mneq(x1M,x2M),insvIM(x3I,x2M)),
    Rule(93	,Value(0.5, Token(93	)),polySite(x3I), Mneq(x0M,x1M),Mneq(x1M,x2M),virtIM(x3I,x2M)),
    Rule(94	,Value(0.5, Token(94	)),polySite(x3I), Mneq(x1M,x2M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
    Rule(95	,Value(0.5, Token(95	)),polySite(x3I), Mneq(x1M,x2M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
    Rule(96	,Value(0.5, Token(96	)),polySite(x3I), Mneq(x2M,x1M),insvIM(x0I,x1M),insvIM(x3I,x2M)),
    Rule(97	,Value(0.5, Token(97	)),polySite(x3I), Mneq(x2M,x1M),insvIM(x0I,x1M),virtIM(x3I,x2M)),
    Rule(98	,Value(0.5, Token(98	)),polySite(x3I), Mneq(x2M,x1M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
    Rule(99	,Value(0.5, Token(99	)),polySite(x3I), Mneq(x2M,x1M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
    Rule(100	,Value(0.5, Token(100	)),polySite(x4I), CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x2M,x0M)),
    Rule(101	,Value(0.5, Token(101	)),polySite(x4I), CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x0I,x1M)),
    Rule(102	,Value(0.5, Token(102	)),polySite(x4I), CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x1M,x2M)),
    Rule(103	,Value(0.5, Token(103	)),polySite(x4I), CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x0I,x1M)),
    Rule(104	,Value(0.5, Token(104	)),polySite(x4I), CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x0I,x1M)),
    Rule(105	,Value(0.5, Token(105	)),polySite(x4I), CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x0I,x1M)),
    Rule(106	,Value(0.5, Token(106	)),polySite(x2I), CICM(x3C,x2I,x4M),Mneq(x0M,x1M),insvIM(x2I,x0M)),
    Rule(107	,Value(0.5, Token(107	)),polySite(x2I), CICM(x3C,x2I,x4M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
    Rule(108	,Value(0.5, Token(108	)),polySite(x2I), Mneq(x0M,x1M),insvIM(x2I,x0M),virtI(x2I)),
    Rule(109	,Value(0.5, Token(109	)),polySite(x2I), Mneq(x0M,x1M),virtI(x2I),virtIM(x2I,x0M)),
    Rule(110	,Value(0.5, Token(110	)),polySite(x3I), CICM(x2C,x3I,x0M),Mneq(x0M,x1M),insvIM(x3I,x4M)),
    Rule(111	,Value(0.5, Token(111	)),polySite(x3I), CICM(x2C,x3I,x0M),Mneq(x0M,x1M),virtIM(x3I,x4M)),
    Rule(112	,Value(0.5, Token(112	)),polySite(x3I), CICM(x2C,x3I,x0M),Mneq(x0M,x1M),virtI(x3I)),
    Rule(113	,Value(0.5, Token(113	)),polySite(x0I), CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x0I,x1M)),
    Rule(114	,Value(0.5, Token(114	)),polySite(x0I), CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x0I,x1M)),
    Rule(115	,Value(0.5, Token(115	)),polySite(x0I), CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x0I,x1M)),
    Rule(116	,Value(0.5, Token(116	)),polySite(x0I), CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x0I,x1M)),
    Rule(117	,Value(0.5, Token(117	)),polySite(x0I), CICM(x3C,x2I,x4M),insvIM(x0I,x1M),insvIM(x2I,x1M)),
    Rule(118	,Value(0.5, Token(118	)),polySite(x0I), CICM(x3C,x2I,x4M),insvIM(x0I,x1M),virtIM(x2I,x1M)),
    Rule(119	,Value(0.5, Token(119	)),polySite(x0I), CICM(x3C,x2I,x4M),insvIM(x2I,x1M),virtIM(x0I,x1M)),
    Rule(120	,Value(0.5, Token(120	)),polySite(x0I), CICM(x3C,x2I,x4M),virtIM(x0I,x1M),virtIM(x2I,x1M)),
    Rule(121	,Value(0.5, Token(121	)),polySite(x0I), insvIM(x0I,x1M),insvIM(x2I,x1M),virtI(x2I)),
    Rule(122	,Value(0.5, Token(122	)),polySite(x0I), insvIM(x0I,x1M),insvIM(x2I,x1M),polySite(x2I)),
    Rule(123	,Value(0.5, Token(123	)),polySite(x0I), insvIM(x0I,x1M),virtI(x2I),virtIM(x2I,x1M)),
    Rule(124	,Value(0.5, Token(124	)),polySite(x0I), insvIM(x0I,x1M),polySite(x2I),virtIM(x2I,x1M)),
    Rule(125	,Value(0.5, Token(125	)),polySite(x0I), insvIM(x2I,x1M),virtI(x2I),virtIM(x0I,x1M)),
    Rule(126	,Value(0.5, Token(126	)),polySite(x0I), insvIM(x2I,x1M),polySite(x2I),virtIM(x0I,x1M)),
    Rule(127	,Value(0.5, Token(127	)),polySite(x0I), virtI(x2I),virtIM(x0I,x1M),virtIM(x2I,x1M)),
    Rule(128	,Value(0.5, Token(128	)),polySite(x0I), polySite(x2I),virtIM(x0I,x1M),virtIM(x2I,x1M)),
    Rule(129	,Value(0.5, Token(129	)),polySite(x4I), CICM(x2C,x0I,x3M),insvIM(x0I,x1M),insvIM(x4I,x3M)),
    Rule(130	,Value(0.5, Token(130	)),polySite(x4I), CICM(x2C,x0I,x3M),insvIM(x0I,x1M),virtIM(x4I,x3M)),
    Rule(131	,Value(0.5, Token(131	)),polySite(x4I), CICM(x2C,x0I,x3M),insvIM(x4I,x3M),virtIM(x0I,x1M)),
    Rule(132	,Value(0.5, Token(132	)),polySite(x4I), CICM(x2C,x0I,x3M),virtIM(x0I,x1M),virtIM(x4I,x3M)),
    Rule(133	,Value(0.5, Token(133	)),polySite(x3I), Mneq(x0M,x0M),Mneq(x0M,x2M),insvIM(x3I,x2M)),
    Rule(134	,Value(0.5, Token(134	)),polySite(x3I), Mneq(x0M,x0M),Mneq(x0M,x2M),virtIM(x3I,x2M)),
    Rule(135	,Value(0.5, Token(135	)),polySite(x3I), Mneq(x0M,x2M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
    Rule(136	,Value(0.5, Token(136	)),polySite(x3I), Mneq(x0M,x2M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
    Rule(137	,Value(0.5, Token(137	)),polySite(x3I), Mneq(x0M,x0M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
    Rule(138	,Value(0.5, Token(138	)),polySite(x3I), Mneq(x0M,x0M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
    Rule(139	,Value(0.5, Token(139	)),polySite(x3I), Mneq(x1M,x2M),insvIM(x3I,x1M),insvIM(x3I,x2M)),
    Rule(140	,Value(0.5, Token(140	)),polySite(x3I), Mneq(x1M,x2M),insvIM(x3I,x1M),virtIM(x3I,x2M)),
    Rule(141	,Value(0.5, Token(141	)),polySite(x3I), Mneq(x1M,x2M),insvIM(x3I,x2M),virtIM(x3I,x1M)),
    Rule(142	,Value(0.5, Token(142	)),polySite(x3I), Mneq(x1M,x2M),virtIM(x3I,x1M),virtIM(x3I,x2M)),
    Rule(143	,Value(0.5, Token(143	)),polySite(x4I), CICM(x3C,x4I,x2M),Mneq(x0M,x2M),Mneq(x2M,x0M)),
    Rule(144	,Value(0.5, Token(144	)),polySite(x4I), CICM(x3C,x4I,x2M),Mneq(x0M,x0M),Mneq(x2M,x0M)),
    Rule(145	,Value(0.5, Token(145	)),polySite(x4I), CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x4I,x1M)),
    Rule(146	,Value(0.5, Token(146	)),polySite(x4I), CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x4I,x1M)),
    Rule(147	,Value(0.5, Token(147	)),polySite(x4I), CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x4I,x1M)),
    Rule(148	,Value(0.5, Token(148	)),polySite(x4I), CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x4I,x1M)),
    Rule(149	,Value(0.5, Token(149	)),polySite(x2I), Mneq(x0M,x0M),insvIM(x2I,x0M),virtIM(x2I,x3M)),
    Rule(150	,Value(0.5, Token(150	)),polySite(x2I), Mneq(x0M,x0M),insvIM(x2I,x3M),virtIM(x2I,x0M)),
    Rule(151	,Value(0.5, Token(151	)),polySite(x2I), CICM(x3C,x2I,x4M),Mneq(x0M,x0M),insvIM(x2I,x0M)),
    Rule(152	,Value(0.5, Token(152	)),polySite(x2I), CICM(x3C,x2I,x4M),Mneq(x0M,x0M),virtIM(x2I,x0M)),
    Rule(153	,Value(0.5, Token(153	)),polySite(x2I), CICM(x3C,x2I,x0M),Mneq(x0M,x1M),insvIM(x2I,x0M)),
    Rule(154	,Value(0.5, Token(154	)),polySite(x2I), CICM(x3C,x2I,x0M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
    Rule(155	,Value(0.5, Token(155	)),polySite(x0I), CICM(x3C,x2I,x1M),insvIM(x0I,x1M),insvIM(x2I,x1M)),
    Rule(156	,Value(0.5, Token(156	)),polySite(x0I), CICM(x3C,x2I,x1M),insvIM(x0I,x1M),virtIM(x2I,x1M)),
    Rule(157	,Value(0.5, Token(157	)),polySite(x0I), CICM(x3C,x2I,x1M),insvIM(x2I,x1M),virtIM(x0I,x1M)),
    Rule(158	,Value(0.5, Token(158	)),polySite(x0I), CICM(x3C,x2I,x1M),virtIM(x0I,x1M),virtIM(x2I,x1M)),
    Rule(159	,Value(0.5, Token(159	)),insvIM(x0I,x1M), virtIM(x0I,x1M)),
    Rule(160	,Value(0.5, Token(160	)),insvIM(x1I,x2M), CICM(x0C,x1I,x2M)),
    Rule(161	,Value(0.5, Token(161	)),insvIM(x3I,x1M), insvIM(x0I,x1M),insvIM(x0I,x2M),insvIM(x3I,x2M)),
    Rule(162	,Value(0.5, Token(162	)),insvIM(x3I,x1M), insvIM(x0I,x1M),insvIM(x0I,x2M),virtIM(x3I,x2M)),
    Rule(163	,Value(0.5, Token(163	)),insvIM(x3I,x1M), insvIM(x0I,x1M),insvIM(x3I,x2M),virtIM(x0I,x2M)),
    Rule(164	,Value(0.5, Token(164	)),insvIM(x3I,x1M), insvIM(x0I,x1M),virtIM(x0I,x2M),virtIM(x3I,x2M)),
    Rule(165	,Value(0.5, Token(165	)),insvIM(x3I,x1M), Mneq(x0M,x1M),Mneq(x0M,x2M),insvIM(x3I,x2M)),
    Rule(166	,Value(0.5, Token(166	)),insvIM(x3I,x1M), Mneq(x0M,x1M),Mneq(x0M,x2M),virtIM(x3I,x2M)),
    Rule(167	,Value(0.5, Token(167	)),insvIM(x3I,x1M), insvIM(x0I,x2M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
    Rule(168	,Value(0.5, Token(168	)),insvIM(x3I,x1M), insvIM(x0I,x2M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
    Rule(169	,Value(0.5, Token(169	)),insvIM(x3I,x1M), insvIM(x3I,x2M),virtIM(x0I,x1M),virtIM(x0I,x2M)),
    Rule(170	,Value(0.5, Token(170	)),insvIM(x3I,x1M), virtIM(x0I,x1M),virtIM(x0I,x2M),virtIM(x3I,x2M)),
    Rule(171	,Value(0.5, Token(171	)),insvIM(x3I,x0M), Mneq(x0M,x1M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
    Rule(172	,Value(0.5, Token(172	)),insvIM(x3I,x0M), Mneq(x0M,x1M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
    Rule(173	,Value(0.5, Token(173	)),insvIM(x3I,x1M), Mneq(x0M,x1M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
    Rule(174	,Value(0.5, Token(174	)),insvIM(x3I,x1M), Mneq(x0M,x1M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
    Rule(175	,Value(0.5, Token(175	)),insvIM(x3I,x0M), Mneq(x0M,x1M),Mneq(x1M,x2M),insvIM(x3I,x2M)),
    Rule(176	,Value(0.5, Token(176	)),insvIM(x3I,x0M), Mneq(x0M,x1M),Mneq(x1M,x2M),virtIM(x3I,x2M)),
    Rule(177	,Value(0.5, Token(177	)),insvIM(x3I,x1M), Mneq(x1M,x2M),insvIM(x0I,x1M),insvIM(x3I,x2M)),
    Rule(178	,Value(0.5, Token(178	)),insvIM(x3I,x1M), Mneq(x1M,x2M),insvIM(x0I,x1M),virtIM(x3I,x2M)),
    Rule(179	,Value(0.5, Token(179	)),insvIM(x3I,x1M), Mneq(x0M,x1M),Mneq(x1M,x2M),insvIM(x3I,x2M)),
    Rule(180	,Value(0.5, Token(180	)),insvIM(x3I,x1M), Mneq(x0M,x1M),Mneq(x1M,x2M),virtIM(x3I,x2M)),
    Rule(181	,Value(0.5, Token(181	)),insvIM(x3I,x1M), Mneq(x1M,x2M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
    Rule(182	,Value(0.5, Token(182	)),insvIM(x3I,x1M), Mneq(x1M,x2M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
    Rule(183	,Value(0.5, Token(183	)),insvIM(x3I,x0M), Mneq(x0M,x1M),Mneq(x2M,x1M),insvIM(x3I,x2M)),
    Rule(184	,Value(0.5, Token(184	)),insvIM(x3I,x0M), Mneq(x0M,x1M),Mneq(x2M,x1M),virtIM(x3I,x2M)),
    Rule(185	,Value(0.5, Token(185	)),insvIM(x3I,x1M), Mneq(x2M,x1M),insvIM(x0I,x1M),insvIM(x3I,x2M)),
    Rule(186	,Value(0.5, Token(186	)),insvIM(x3I,x1M), Mneq(x2M,x1M),insvIM(x0I,x1M),virtIM(x3I,x2M)),
    Rule(187	,Value(0.5, Token(187	)),insvIM(x3I,x1M), Mneq(x2M,x1M),insvIM(x3I,x2M),virtIM(x0I,x1M)),
    Rule(188	,Value(0.5, Token(188	)),insvIM(x3I,x1M), Mneq(x2M,x1M),virtIM(x0I,x1M),virtIM(x3I,x2M)),
    Rule(189	,Value(0.5, Token(189	)),insvIM(x4I,x1M), CICM(x3C,x4I,x2M),insvIM(x0I,x1M),insvIM(x0I,x2M)),
    Rule(190	,Value(0.5, Token(190	)),insvIM(x4I,x1M), CICM(x3C,x4I,x2M),insvIM(x0I,x1M),virtIM(x0I,x2M)),
    Rule(191	,Value(0.5, Token(191	)),insvIM(x4I,x1M), CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x0M,x2M)),
    Rule(192	,Value(0.5, Token(192	)),insvIM(x4I,x1M), CICM(x3C,x4I,x2M),insvIM(x0I,x2M),virtIM(x0I,x1M)),
    Rule(193	,Value(0.5, Token(193	)),insvIM(x4I,x1M), CICM(x3C,x4I,x2M),virtIM(x0I,x1M),virtIM(x0I,x2M)),
    Rule(194	,Value(0.5, Token(194	)),insvIM(x4I,x0M), CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x2M,x0M)),
    Rule(195	,Value(0.5, Token(195	)),insvIM(x4I,x1M), CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x2M,x0M)),
    Rule(196	,Value(0.5, Token(196	)),insvIM(x4I,x0M), CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x1M,x2M)),
    Rule(197	,Value(0.5, Token(197	)),insvIM(x4I,x1M), CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x0I,x1M)),
    Rule(198	,Value(0.5, Token(198	)),insvIM(x4I,x1M), CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x1M,x2M)),
    Rule(199	,Value(0.5, Token(199	)),insvIM(x4I,x1M), CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x0I,x1M)),
    Rule(200	,Value(0.5, Token(200	)),insvIM(x4I,x0M), CICM(x3C,x4I,x2M),Mneq(x0M,x1M),Mneq(x2M,x1M)),
    Rule(201	,Value(0.5, Token(201	)),insvIM(x4I,x1M), CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x0I,x1M)),
    Rule(202	,Value(0.5, Token(202	)),insvIM(x4I,x1M), CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x0I,x1M)),
    Rule(203	,Value(0.5, Token(203	)),insvIM(x2I,x0M), Mneq(x0M,x1M),insvIM(x2I,x3M),virtIM(x2I,x0M)),
    Rule(204	,Value(0.5, Token(204	)),insvIM(x2I,x1M), insvIM(x0I,x1M),insvIM(x2I,x3M),virtIM(x2I,x1M)),
    Rule(205	,Value(0.5, Token(205	)),insvIM(x2I,x1M), Mneq(x0M,x1M),insvIM(x2I,x3M),virtIM(x2I,x1M)),
    Rule(206	,Value(0.5, Token(206	)),insvIM(x2I,x0M), CICM(x3C,x2I,x4M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
    Rule(207	,Value(0.5, Token(207	)),insvIM(x2I,x1M), CICM(x3C,x2I,x4M),Mneq(x0M,x1M),insvIM(x2I,x0M)),
    Rule(208	,Value(0.5, Token(208	)),insvIM(x2I,x1M), CICM(x3C,x2I,x4M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
    Rule(209	,Value(0.5, Token(209	)),insvIM(x2I,x1M), CICM(x3C,x2I,x4M),insvIM(x0I,x1M),virtIM(x2I,x1M)),
    Rule(210	,Value(0.5, Token(210	)),insvIM(x2I,x1M), CICM(x3C,x2I,x4M),Mneq(x0M,x1M),virtIM(x2I,x1M)),
    Rule(211	,Value(0.5, Token(211	)),insvIM(x2I,x0M), Mneq(x0M,x1M),virtI(x2I),virtIM(x2I,x0M)),
    Rule(212	,Value(0.5, Token(212	)),insvIM(x2I,x0M), Mneq(x0M,x1M),polySite(x2I),virtIM(x2I,x0M)),
    Rule(213	,Value(0.5, Token(213	)),insvIM(x2I,x1M), Mneq(x0M,x1M),insvIM(x2I,x0M),virtI(x2I)),
    Rule(214	,Value(0.5, Token(214	)),insvIM(x2I,x1M), Mneq(x0M,x1M),insvIM(x2I,x0M),polySite(x2I)),
    Rule(215	,Value(0.5, Token(215	)),insvIM(x2I,x1M), Mneq(x0M,x1M),virtI(x2I),virtIM(x2I,x0M)),
    Rule(216	,Value(0.5, Token(216	)),insvIM(x2I,x1M), Mneq(x0M,x1M),polySite(x2I),virtIM(x2I,x0M)),
    Rule(217	,Value(0.5, Token(217	)),insvIM(x2I,x1M), insvIM(x0I,x1M),virtI(x2I),virtIM(x2I,x1M)),
    Rule(218	,Value(0.5, Token(218	)),insvIM(x2I,x1M), insvIM(x0I,x1M),polySite(x2I),virtIM(x2I,x1M)),
    Rule(219	,Value(0.5, Token(219	)),insvIM(x2I,x1M), Mneq(x0M,x1M),virtI(x2I),virtIM(x2I,x1M)),
    Rule(220	,Value(0.5, Token(220	)),insvIM(x2I,x1M), Mneq(x0M,x1M),polySite(x2I),virtIM(x2I,x1M)),
    Rule(221	,Value(0.5, Token(221	)),insvIM(x3I,x0M), CICM(x2C,x3I,x0M),Mneq(x0M,x1M),insvIM(x3I,x4M)),
    Rule(222	,Value(0.5, Token(222	)),insvIM(x3I,x0M), CICM(x2C,x3I,x0M),Mneq(x0M,x1M),virtIM(x3I,x4M)),
    Rule(223	,Value(0.5, Token(223	)),insvIM(x3I,x1M), CICM(x2C,x3I,x0M),Mneq(x0M,x1M),insvIM(x3I,x4M)),
    Rule(224	,Value(0.5, Token(224	)),insvIM(x3I,x1M), CICM(x2C,x3I,x0M),Mneq(x0M,x1M),virtIM(x3I,x4M)),
    Rule(225	,Value(0.5, Token(225	)),insvIM(x3I,x1M), CICM(x2C,x3I,x1M),insvIM(x0I,x1M),insvIM(x3I,x4M)),
    Rule(226	,Value(0.5, Token(226	)),insvIM(x3I,x1M), CICM(x2C,x3I,x1M),insvIM(x0I,x1M),virtIM(x3I,x4M)),
    Rule(227	,Value(0.5, Token(227	)),insvIM(x3I,x1M), CICM(x2C,x3I,x1M),Mneq(x0M,x1M),insvIM(x3I,x4M)),
    Rule(228	,Value(0.5, Token(228	)),insvIM(x3I,x1M), CICM(x2C,x3I,x1M),Mneq(x0M,x1M),virtIM(x3I,x4M)),
    Rule(229	,Value(0.5, Token(229	)),insvIM(x3I,x1M), CICM(x2C,x3I,x1M),insvIM(x3I,x4M),virtIM(x0I,x1M)),
    Rule(230	,Value(0.5, Token(230	)),insvIM(x3I,x1M), CICM(x2C,x3I,x1M),virtIM(x0I,x1M),virtIM(x3I,x4M)),
    Rule(231	,Value(0.5, Token(231	)),insvIM(x4I,x1M), CICM(x2C,x3I,x0M),CICM(x2C,x4I,x5M),Mneq(x0M,x1M)),
    Rule(232	,Value(0.5, Token(232	)),insvIM(x3I,x0M), CICM(x2C,x3I,x0M),Mneq(x0M,x1M),virtI(x3I)),
    Rule(233	,Value(0.5, Token(233	)),insvIM(x3I,x0M), CICM(x2C,x3I,x0M),Mneq(x0M,x1M),polySite(x3I)),
    Rule(234	,Value(0.5, Token(234	)),insvIM(x3I,x1M), CICM(x2C,x3I,x0M),Mneq(x0M,x1M),virtI(x3I)),
    Rule(235	,Value(0.5, Token(235	)),insvIM(x3I,x1M), CICM(x2C,x3I,x0M),Mneq(x0M,x1M),polySite(x3I)),
    Rule(236	,Value(0.5, Token(236	)),insvIM(x3I,x1M), CICM(x2C,x3I,x1M),insvIM(x0I,x1M),virtI(x3I)),
    Rule(237	,Value(0.5, Token(237	)),insvIM(x3I,x1M), CICM(x2C,x3I,x1M),insvIM(x0I,x1M),polySite(x3I)),
    Rule(238	,Value(0.5, Token(238	)),insvIM(x3I,x1M), CICM(x2C,x3I,x1M),Mneq(x0M,x1M),virtI(x3I)),
    Rule(239	,Value(0.5, Token(239	)),insvIM(x3I,x1M), CICM(x2C,x3I,x1M),Mneq(x0M,x1M),polySite(x3I)),
    Rule(240	,Value(0.5, Token(240	)),insvIM(x3I,x1M), CICM(x2C,x3I,x1M),virtI(x3I),virtIM(x0I,x1M)),
    Rule(241	,Value(0.5, Token(241	)),insvIM(x3I,x1M), CICM(x2C,x3I,x1M),polySite(x3I),virtIM(x0I,x1M)),
    Rule(242	,Value(0.5, Token(242	)),insvIM(x0I,x2M), CICM(x3C,x4I,x2M),Mneq(x1M,x2M),insvIM(x0I,x1M)),
    Rule(243	,Value(0.5, Token(243	)),insvIM(x0I,x2M), CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x0I,x1M)),
    Rule(244	,Value(0.5, Token(244	)),insvIM(x0I,x2M), CICM(x3C,x4I,x2M),Mneq(x2M,x1M),insvIM(x0I,x1M)),
    Rule(245	,Value(0.5, Token(245	)),insvIM(x0I,x2M), CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x0I,x1M)),
    Rule(246	,Value(0.5, Token(246	)),insvIM(x0I,x2M), CICM(x3C,x4I,x2M),insvIM(x0I,x1M),virtIM(x0I,x2M)),
    Rule(247	,Value(0.5, Token(247	)),insvIM(x0I,x4M), CICM(x3C,x2I,x4M),insvIM(x0I,x1M),insvIM(x2I,x1M)),
    Rule(248	,Value(0.5, Token(248	)),insvIM(x0I,x4M), CICM(x3C,x2I,x4M),insvIM(x0I,x1M),virtIM(x2I,x1M)),
    Rule(249	,Value(0.5, Token(249	)),insvIM(x0I,x4M), CICM(x3C,x2I,x4M),insvIM(x2I,x1M),virtIM(x0I,x1M)),
    Rule(250	,Value(0.5, Token(250	)),insvIM(x0I,x4M), CICM(x3C,x2I,x4M),virtIM(x0I,x1M),virtIM(x2I,x1M)),
    Rule(251	,Value(0.5, Token(251	)),insvIM(x4I,x1M), CICM(x2C,x0I,x3M),insvIM(x0I,x1M),insvIM(x4I,x3M)),
    Rule(252	,Value(0.5, Token(252	)),insvIM(x4I,x1M), CICM(x2C,x0I,x3M),insvIM(x0I,x1M),virtIM(x4I,x3M)),
    Rule(253	,Value(0.5, Token(253	)),insvIM(x4I,x1M), CICM(x2C,x0I,x3M),insvIM(x4I,x3M),virtIM(x0I,x1M)),
    Rule(254	,Value(0.5, Token(254	)),insvIM(x4I,x1M), CICM(x2C,x0I,x3M),virtIM(x0I,x1M),virtIM(x4I,x3M)),
    Rule(255	,Value(0.5, Token(255	)),insvIM(x4I,x1M), CICM(x2C,x0I,x3M),CICM(x2C,x4I,x5M),insvIM(x0I,x1M)),
    Rule(256	,Value(0.5, Token(256	)),insvIM(x4I,x1M), CICM(x2C,x0I,x3M),CICM(x2C,x4I,x5M),virtIM(x0I,x1M)),
    Rule(257	,Value(0.5, Token(257	)),insvIM(x5I,x1M), CICM(x2C,x0I,x3M),CICM(x4C,x5I,x3M),insvIM(x0I,x1M)),
    Rule(258	,Value(0.5, Token(258	)),insvIM(x5I,x1M), CICM(x2C,x0I,x3M),CICM(x4C,x5I,x3M),virtIM(x0I,x1M)),
    Rule(259	,Value(0.5, Token(259	)),insvIM(x0I,x5M), CICM(x2C,x3I,x1M),CICM(x4C,x3I,x5M),insvIM(x0I,x1M)),
    Rule(260	,Value(0.5, Token(260	)),insvIM(x0I,x5M), CICM(x2C,x3I,x1M),CICM(x4C,x3I,x5M),virtIM(x0I,x1M)),
    Rule(261	,Value(0.5, Token(261	)),insvIM(x1I,x3M), CICM(x0C,x1I,x2M),CICM(x4C,x5I,x3M),Mneq(x2M,x3M)),
    Rule(262	,Value(0.5, Token(262	)),insvIM(x3I,x2M), CICM(x0C,x1I,x2M),virtI(x3I),virtIM(x3I,x2M)),
    Rule(263	,Value(0.5, Token(263	)),insvIM(x3I,x2M), CICM(x0C,x1I,x2M),polySite(x3I),virtIM(x3I,x2M)),
    Rule(264	,Value(0.5, Token(264	)),insvIM(x3I,x0M), Mneq(x0M,x2M),Mneq(x2M,x0M),insvIM(x3I,x2M)),
    Rule(265	,Value(0.5, Token(265	)),insvIM(x3I,x0M), Mneq(x0M,x2M),Mneq(x2M,x0M),virtIM(x3I,x2M)),
    Rule(266	,Value(0.5, Token(266	)),insvIM(x3I,x1M), Mneq(x1M,x2M),insvIM(x3I,x2M),virtIM(x3I,x1M)),
    Rule(267	,Value(0.5, Token(267	)),insvIM(x3I,x1M), Mneq(x1M,x2M),virtIM(x3I,x1M),virtIM(x3I,x2M)),
    Rule(268	,Value(0.5, Token(268	)),insvIM(x3I,x1M), Mneq(x2M,x1M),insvIM(x3I,x2M),virtIM(x3I,x1M)),
    Rule(269	,Value(0.5, Token(269	)),insvIM(x3I,x1M), Mneq(x2M,x1M),virtIM(x3I,x1M),virtIM(x3I,x2M)),
    Rule(270	,Value(0.5, Token(270	)),insvIM(x4I,x0M), CICM(x3C,x4I,x2M),Mneq(x0M,x2M),Mneq(x2M,x0M)),
    Rule(271	,Value(0.5, Token(271	)),insvIM(x4I,x1M), CICM(x3C,x4I,x2M),Mneq(x1M,x2M),virtIM(x4I,x1M)),
    Rule(272	,Value(0.5, Token(272	)),insvIM(x4I,x1M), CICM(x3C,x4I,x2M),Mneq(x2M,x1M),virtIM(x4I,x1M)),
    Rule(273	,Value(0.5, Token(273	)),insvIM(x2I,x1M), CICM(x3C,x2I,x0M),Mneq(x0M,x1M),insvIM(x2I,x0M)),
    Rule(274	,Value(0.5, Token(274	)),insvIM(x2I,x1M), CICM(x3C,x2I,x0M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
    Rule(275	,Value(0.5, Token(275	)),insvIM(x2I,x1M), CICM(x3C,x2I,x1M),Mneq(x0M,x1M),insvIM(x2I,x0M)),
    Rule(276	,Value(0.5, Token(276	)),insvIM(x2I,x1M), CICM(x3C,x2I,x1M),Mneq(x0M,x1M),virtIM(x2I,x0M)),
    Rule(277	,Value(0.5, Token(277	)),insvIM(x2I,x0M), CICM(x3C,x2I,x0M),Mneq(x0M,x1M),insvIM(x2I,x1M)),
    Rule(278	,Value(0.5, Token(278	)),insvIM(x2I,x0M), CICM(x3C,x2I,x0M),Mneq(x0M,x1M),virtIM(x2I,x1M)),
    Rule(279	,Value(0.5, Token(279	)),insvIM(x4I,x1M), CICM(x2C,x3I,x0M),CICM(x2C,x4I,x1M),Mneq(x0M,x1M)),
    Rule(280	,Value(0.5, Token(280	)),insvIM(x3I,x1M), CICM(x2C,x3I,x0M),CICM(x4C,x3I,x1M),Mneq(x0M,x1M)),
    Rule(281	,Value(0.5, Token(281	)),insvIM(x4I,x1M), CICM(x2C,x3I,x1M),CICM(x2C,x4I,x5M),insvIM(x3I,x1M)),
    Rule(282	,Value(0.5, Token(282	)),insvIM(x4I,x1M), CICM(x2C,x3I,x1M),CICM(x2C,x4I,x5M),virtIM(x3I,x1M)),
    Rule(283	,Value(0.5, Token(283	)),insvIM(x4I,x1M), CICM(x2C,x3I,x1M),CICM(x2C,x4I,x5M),Mneq(x5M,x1M)),
    Rule(284	,Value(0.5, Token(284	)),insvIM(x4I,x1M), CICM(x2C,x0I,x5M),CICM(x2C,x4I,x5M),insvIM(x0I,x1M)),
    Rule(285	,Value(0.5, Token(285	)),insvIM(x4I,x1M), CICM(x2C,x0I,x5M),CICM(x2C,x4I,x5M),virtIM(x0I,x1M)),
    Rule(286	,Value(0.5, Token(286	)),insvIM(x4I,x1M), CICM(x2C,x0I,x3M),CICM(x2C,x4I,x1M),insvIM(x0I,x1M)),
    Rule(287	,Value(0.5, Token(287	)),insvIM(x4I,x1M), CICM(x2C,x0I,x3M),CICM(x2C,x4I,x1M),virtIM(x0I,x1M)),
    Rule(288	,Value(0.5, Token(288	)),insvIM(x0I,x5M), CICM(x2C,x4I,x1M),CICM(x2C,x4I,x5M),insvIM(x0I,x1M)),
    Rule(289	,Value(0.5, Token(289	)),insvIM(x0I,x5M), CICM(x2C,x4I,x1M),CICM(x2C,x4I,x5M),virtIM(x0I,x1M)),
  )

  val soupProg: Program = Program("PolysiteSoup", soup)
  val evaluator = SeminaiveEvaluator(soupProg)

  test(s"Applying evaluator ${evaluator.name} to program ${soupProg.name}") {
    val startTime = System.nanoTime()
    val idb = evaluator(edb)
    val endTime = System.nanoTime()
    println(s"A ${idb(insvIM).support.size}. ${(endTime - startTime) / 1.0e9}")
  }

}