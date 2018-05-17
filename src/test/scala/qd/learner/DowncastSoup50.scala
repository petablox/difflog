package qd
package learner

import org.scalatest.Ignore

class DowncastSoup50 extends Problem {
  override val name: String = "Downcast"

  val HSet = Range(0, 5).map(i => Atom(i)).toSet
  val H = Domain("H", HSet)

  val MSet = Range(0, 7).map(i => Atom(i)).toSet
  val M = Domain("M", MSet)

  val TSet = Range(0, 7).map(i => Atom(i)).toSet
  val T = Domain("T", TSet)

  val VSet = Range(0, 42).map(i => Atom(i)).toSet
  val V = Domain("V", VSet)

  val VH = Relation("VH", V, H)
  val HT = Relation("HT", H, T)
  val McheckCastInst = Relation("McheckCastInst", M, V, T, V)
  val reachableCast = Relation("reachableCast", T, V)
  val ptsVT = Relation("ptsVT", V, T)
  val unsafeDowncast = Relation("unsafeDowncast", V, T)
  val notSub = Relation("notSub", T, T)
  val badCast = Relation("badCast", V, T)

  val reachableCastTuples = Set((3,34),(3,38),(2,17),(4,25),(4,29)).map{ case (a,b) => DTuple(Atom(a), Atom(b))}
  val VHTuples = Set((0,4),(0,2),(0,1),(0,3),(32,3),(16,3),(8,2),(40,3),(24,2),(36,3),
    (20,4),(12,3),(28,3),(2,4),(2,2),(34,2),(34,1),(34,3),(18,2),(18,3),
    (10,2),(26,4),(26,2),(6,1),(38,3),(22,4),(1,3),(33,2),(17,2),(17,3),
    (9,2),(41,4),(41,2),(41,1),(41,3),(25,4),(25,2),(5,1),(37,3),(21,4),
    (13,3),(29,1),(29,3),(35,3),(19,2),(19,3),(11,3),(27,4),(27,2),
    (7,1),(39,3),(23,4),(15,2)).map{ case (a,b) => DTuple(Atom(a), Atom(b))}
  val HTTuples = Set((4,4),(2,4),(1,2),(3,3)).map{ case (a,b) => DTuple(Atom(a), Atom(b))}
  val McheckCastInstTuples = Set((4,18,2,17),(4,26,4,25),(4,30,4,29),(4,35,3,34),(4,39,3,38)).map {
    case (a,b,c,d) => DTuple(Atom(a), Atom(b), Atom(c), Atom(d))
  }
  val ptsVTTuples = Set((0,4),(0,2),(0,3),(32,3),(16,3),(8,4),(40,3),(24,4),(36,3),(20,4),
    (12,3),(28,3),(2,4),(34,4),(34,2),(34,3),(18,4),(18,3),(10,4),
    (26,4),(6,2),(38,3),(22,4),(1,3),(33,4),(17,4),(17,3),(9,4),
    (41,4),(41,2),(41,3),(25,4),(5,2),(37,3),(21,4),(13,3),(15,4),
    (29,2),(29,3),(35,3),(19,4),(19,3),(11,3),(27,4),(7,2), (39,3),(23,4)).map{ case (a,b) => DTuple(Atom(a), Atom(b))}
  val unsafeDowncastTuples = Set((34,3),(29,4)).map{ case (a,b) => DTuple(Atom(a), Atom(b))}
  val notSubTuples = Set((0,0),(2,0),(2,3),(2,4),(2,5),(3,0),(3,4),(3,5),(4,0),(4,3),(4,5),
    (5,0),(5,2),(5,3),(5,4),(6,0),(6,2),(6,3),(6,4),(6,5)).map{ case (a,b) => DTuple(Atom(a), Atom(b))}
  val badCastTuples = Set((0,0),(34,0),(6,0),(41,0),(5,0),(29,0),(7,0),(0,3),(34,3),(6,3),(41,3),
    (5,3),(29,3),(7,3),(0,4),(34,4),(6,4),(41,4),(5,4),(29,4),(7,4),(0,5),
    (34,5),(6,5),(41,5),(5,5),(29,5),(7,5),(32,0),(16,0),(40,0),(36,0),(12,0),
    (28,0),(18,0),(38,0),(1,0),(17,0),(37,0),(13,0),(35,0),(19,0),(11,0),
    (39,0),(32,4),(16,4),(40,4),(36,4),(12,4),(28,4),(18,4),(38,4),(1,4),
    (17,4),(37,4),(13,4),(35,4),(19,4),(11,4),(39,4),(32,5),(16,5),(40,5),
    (36,5),(12,5),(28,5),(18,5),(38,5),(1,5),(17,5),(37,5),(13,5),(35,5),
    (19,5),(11,5),(39,5),(8,0),(24,0),(20,0),(2,0),(10,0),(26,0),(22,0),(33,0),
    (9,0),(25,0),(21,0),(27,0),(23,0),(15,0),(8,3),(24,3),(20,3),(2,3),(18,3),
    (10,3),(26,3),(22,3),(33,3),(17,3),(9,3),(25,3),(21,3),(19,3),(27,3),(23,3),
    (15,3),(8,5),(24,5),(20,5),(2,5),(10,5),(26,5),(22,5),(33,5),(9,5),(25,5), (21,5),(27,5),(23,5),(15,5)).map {
    case (a,b) => DTuple(Atom(a), Atom(b))
  }

  override val edb : Config = Config(
    VH -> (Instance(VH) ++ VHTuples.map(t => t -> One).toMap),
    HT -> (Instance(HT) ++ HTTuples.map(t => t -> One).toMap),
    McheckCastInst -> (Instance(McheckCastInst) ++ McheckCastInstTuples.map(t => t -> One).toMap),
    notSub -> (Instance(notSub) ++ notSubTuples.map(t => t -> One).toMap),
  )

  override val refOut : Config = Config (
    badCast -> (Instance(badCast) ++ badCastTuples.map(t => t -> One).toMap),
    unsafeDowncast -> (Instance(unsafeDowncast) ++ unsafeDowncastTuples.map(t => t -> One).toMap),
    ptsVT -> (Instance(ptsVT) ++ ptsVTTuples.map(t => t -> One).toMap),
    reachableCast -> (Instance(reachableCast) ++ reachableCastTuples.map(t => t -> One).toMap),
  )

  val x0H : Variable = Variable("x0H", H)
  val x1H : Variable = Variable("x1H", H)
  val x2H : Variable = Variable("x2H", H)
  val x3H : Variable = Variable("x3H", H)
  val x4H : Variable = Variable("x4H", H)

  val x0M : Variable = Variable("x0M", M)
  val x1M : Variable = Variable("x1M", M)
  val x2M : Variable = Variable("x2M", M)
  val x3M : Variable = Variable("x3M", M)
  val x4M : Variable = Variable("x4M", M)

  val x0V : Variable = Variable("x0V", V)
  val x1V : Variable = Variable("x1V", V)
  val x2V : Variable = Variable("x2V", V)
  val x3V : Variable = Variable("x3V", V)
  val x4V : Variable = Variable("x4V", V)

  val x0T : Variable = Variable("x0T", T)
  val x1T : Variable = Variable("x1T", T)
  val x2T : Variable = Variable("x2T", T)
  val x3T : Variable = Variable("x3T", T)
  val x4T : Variable = Variable("x4T", T)

  // Expected: 20, 104, 166, 307
  override val soup : Set[Rule] = Set(
    Rule(1	,Value(0.5, Token(1	)),badCast(x1V,x2T), McheckCastInst(x0M,x1V,x2T,x3V)),
    Rule(2	,Value(0.5, Token(2	)),badCast(x3V,x2T), McheckCastInst(x0M,x1V,x2T,x3V)),
    Rule(3	,Value(0.5, Token(3	)),badCast(x0V,x1T), ptsVT(x0V,x1T)),
    Rule(4	,Value(0.5, Token(4	)),badCast(x0V,x1T), unsafeDowncast(x0V,x1T)),
    Rule(5	,Value(0.5, Token(5	)),badCast(x1V,x0T), reachableCast(x0T,x1V)),
    Rule(6	,Value(0.5, Token(6	)),badCast(x3V,x0T), McheckCastInst(x2M,x3V,x0T,x4V),reachableCast(x0T,x1V)),
    Rule(7	,Value(0.5, Token(7	)),badCast(x3V,x0T), McheckCastInst(x2M,x3V,x0T,x4V),notSub(x0T,x1T)),
    Rule(8	,Value(0.5, Token(8	)),badCast(x3V,x1T), McheckCastInst(x2M,x3V,x0T,x4V),notSub(x0T,x1T)),
    Rule(9	,Value(0.5, Token(9	)),badCast(x4V,x1T), McheckCastInst(x2M,x3V,x0T,x4V),notSub(x0T,x1T)),
    Rule(10	,Value(0.5, Token(10	)),badCast(x3V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),badCast(x0V,x1T)),
    Rule(11	,Value(0.5, Token(11	)),badCast(x3V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x0V,x1T)),
    Rule(12	,Value(0.5, Token(12	)),badCast(x3V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x0V,x1T)),
    Rule(13	,Value(0.5, Token(13	)),badCast(x3V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),notSub(x0T,x1T)),
    Rule(14	,Value(0.5, Token(14	)),badCast(x3V,x1T), HT(x0H,x1T),McheckCastInst(x2M,x3V,x1T,x4V)),
    Rule(15	,Value(0.5, Token(15	)),badCast(x2V,x0T), ptsVT(x2V,x0T),reachableCast(x0T,x1V)),
    Rule(16	,Value(0.5, Token(16	)),badCast(x2V,x0T), reachableCast(x0T,x1V),unsafeDowncast(x2V,x0T)),
    Rule(17	,Value(0.5, Token(17	)),badCast(x2V,x0T), notSub(x0T,x1T),ptsVT(x2V,x0T)),
    Rule(18	,Value(0.5, Token(18	)),badCast(x2V,x0T), notSub(x0T,x1T),unsafeDowncast(x2V,x0T)),
    Rule(19	,Value(0.5, Token(19	)),badCast(x2V,x1T), badCast(x2V,x0T),notSub(x0T,x1T)),
    Rule(20	,Value(0.5, Token(20	)),badCast(x2V,x1T), notSub(x0T,x1T),ptsVT(x2V,x0T)),
    Rule(21	,Value(0.5, Token(21	)),badCast(x2V,x1T), notSub(x0T,x1T),unsafeDowncast(x2V,x0T)),
    Rule(22	,Value(0.5, Token(22	)),badCast(x2V,x1T), HT(x0H,x1T),VH(x2V,x0H)),
    Rule(23	,Value(0.5, Token(23	)),badCast(x2V,x0T), badCast(x2V,x1T),notSub(x0T,x1T)),
    Rule(24	,Value(0.5, Token(24	)),badCast(x2V,x0T), notSub(x0T,x1T),ptsVT(x2V,x1T)),
    Rule(25	,Value(0.5, Token(25	)),badCast(x2V,x0T), notSub(x0T,x1T),unsafeDowncast(x2V,x1T)),
    Rule(26	,Value(0.5, Token(26	)),badCast(x2V,x1T), badCast(x0V,x1T),ptsVT(x2V,x1T)),
    Rule(27	,Value(0.5, Token(27	)),badCast(x2V,x1T), badCast(x0V,x1T),unsafeDowncast(x2V,x1T)),
    Rule(28	,Value(0.5, Token(28	)),badCast(x2V,x1T), ptsVT(x0V,x1T),unsafeDowncast(x2V,x1T)),
    Rule(29	,Value(0.5, Token(29	)),badCast(x2V,x1T), ptsVT(x2V,x1T),unsafeDowncast(x0V,x1T)),
    Rule(30	,Value(0.5, Token(30	)),badCast(x2V,x1T), notSub(x0T,x1T),ptsVT(x2V,x1T)),
    Rule(31	,Value(0.5, Token(31	)),badCast(x2V,x1T), notSub(x0T,x1T),unsafeDowncast(x2V,x1T)),
    Rule(32	,Value(0.5, Token(32	)),badCast(x2V,x1T), HT(x0H,x1T),ptsVT(x2V,x1T)),
    Rule(33	,Value(0.5, Token(33	)),badCast(x2V,x1T), HT(x0H,x1T),unsafeDowncast(x2V,x1T)),
    Rule(34	,Value(0.5, Token(34	)),badCast(x2V,x0T), notSub(x0T,x1T),reachableCast(x0T,x2V)),
    Rule(35	,Value(0.5, Token(35	)),badCast(x2V,x1T), notSub(x0T,x1T),reachableCast(x0T,x2V)),
    Rule(36	,Value(0.5, Token(36	)),badCast(x2V,x0T), notSub(x0T,x1T),reachableCast(x1T,x2V)),
    Rule(37	,Value(0.5, Token(37	)),badCast(x2V,x1T), badCast(x0V,x1T),reachableCast(x1T,x2V)),
    Rule(38	,Value(0.5, Token(38	)),badCast(x2V,x1T), ptsVT(x0V,x1T),reachableCast(x1T,x2V)),
    Rule(39	,Value(0.5, Token(39	)),badCast(x2V,x1T), reachableCast(x1T,x2V),unsafeDowncast(x0V,x1T)),
    Rule(40	,Value(0.5, Token(40	)),badCast(x2V,x1T), notSub(x0T,x1T),reachableCast(x1T,x2V)),
    Rule(41	,Value(0.5, Token(41	)),badCast(x2V,x1T), HT(x0H,x1T),reachableCast(x1T,x2V)),
    Rule(42	,Value(0.5, Token(42	)),badCast(x0V,x3T), McheckCastInst(x2M,x0V,x3T,x4V),badCast(x0V,x1T)),
    Rule(43	,Value(0.5, Token(43	)),badCast(x0V,x3T), McheckCastInst(x2M,x0V,x3T,x4V),ptsVT(x0V,x1T)),
    Rule(44	,Value(0.5, Token(44	)),badCast(x0V,x3T), McheckCastInst(x2M,x0V,x3T,x4V),unsafeDowncast(x0V,x1T)),
    Rule(45	,Value(0.5, Token(45	)),badCast(x0V,x3T), McheckCastInst(x2M,x0V,x3T,x4V),VH(x0V,x1H)),
    Rule(46	,Value(0.5, Token(46	)),badCast(x0V,x4T), McheckCastInst(x2M,x3V,x4T,x0V),badCast(x0V,x1T)),
    Rule(47	,Value(0.5, Token(47	)),badCast(x0V,x4T), McheckCastInst(x2M,x3V,x4T,x0V),ptsVT(x0V,x1T)),
    Rule(48	,Value(0.5, Token(48	)),badCast(x0V,x4T), McheckCastInst(x2M,x3V,x4T,x0V),unsafeDowncast(x0V,x1T)),
    Rule(49	,Value(0.5, Token(49	)),badCast(x0V,x4T), McheckCastInst(x2M,x3V,x4T,x0V),VH(x0V,x1H)),
    Rule(50	,Value(0.5, Token(50	)),badCast(x0V,x2T), badCast(x0V,x1T),reachableCast(x2T,x0V)),
    Rule(51	,Value(0.5, Token(51	)),badCast(x0V,x2T), ptsVT(x0V,x1T),reachableCast(x2T,x0V)),
    Rule(52	,Value(0.5, Token(52	)),badCast(x0V,x2T), reachableCast(x2T,x0V),unsafeDowncast(x0V,x1T)),
    Rule(53	,Value(0.5, Token(53	)),badCast(x0V,x2T), VH(x0V,x1H),reachableCast(x2T,x0V)),
    Rule(54	,Value(0.5, Token(54	)),badCast(x1V,x4T), McheckCastInst(x0M,x1V,x2T,x3V),ptsVT(x1V,x4T)),
    Rule(55	,Value(0.5, Token(55	)),badCast(x1V,x4T), McheckCastInst(x0M,x1V,x2T,x3V),unsafeDowncast(x1V,x4T)),
    Rule(56	,Value(0.5, Token(56	)),badCast(x3V,x4T), McheckCastInst(x0M,x1V,x2T,x3V),badCast(x1V,x4T)),
    Rule(57	,Value(0.5, Token(57	)),badCast(x3V,x4T), McheckCastInst(x0M,x1V,x2T,x3V),ptsVT(x1V,x4T)),
    Rule(58	,Value(0.5, Token(58	)),badCast(x3V,x4T), McheckCastInst(x0M,x1V,x2T,x3V),unsafeDowncast(x1V,x4T)),
    Rule(59	,Value(0.5, Token(59	)),badCast(x4V,x2T), McheckCastInst(x0M,x1V,x2T,x3V),ptsVT(x4V,x2T)),
    Rule(60	,Value(0.5, Token(60	)),badCast(x4V,x2T), McheckCastInst(x0M,x1V,x2T,x3V),unsafeDowncast(x4V,x2T)),
    Rule(61	,Value(0.5, Token(61	)),badCast(x1V,x4T), McheckCastInst(x0M,x1V,x2T,x3V),badCast(x3V,x4T)),
    Rule(62	,Value(0.5, Token(62	)),badCast(x1V,x4T), McheckCastInst(x0M,x1V,x2T,x3V),ptsVT(x3V,x4T)),
    Rule(78	,Value(0.5, Token(78	)),badCast(x4V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x3V,x1T)),
    Rule(79	,Value(0.5, Token(79	)),badCast(x4V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x4V,x1T)),
    Rule(80	,Value(0.5, Token(80	)),badCast(x4V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x4V,x1T)),
    Rule(81	,Value(0.5, Token(81	)),badCast(x2V,x0T), ptsVT(x2V,x0T),reachableCast(x0T,x2V)),
    Rule(82	,Value(0.5, Token(82	)),badCast(x2V,x0T), reachableCast(x0T,x2V),unsafeDowncast(x2V,x0T)),
    Rule(83	,Value(0.5, Token(83	)),ptsVT(x1V,x2T), McheckCastInst(x0M,x1V,x2T,x3V)),
    Rule(84	,Value(0.5, Token(84	)),ptsVT(x3V,x2T), McheckCastInst(x0M,x1V,x2T,x3V)),
    Rule(85	,Value(0.5, Token(85	)),ptsVT(x0V,x1T), badCast(x0V,x1T)),
    Rule(86	,Value(0.5, Token(86	)),ptsVT(x0V,x1T), unsafeDowncast(x0V,x1T)),
    Rule(87	,Value(0.5, Token(87	)),ptsVT(x1V,x0T), reachableCast(x0T,x1V)),
    Rule(88	,Value(0.5, Token(88	)),ptsVT(x3V,x0T), McheckCastInst(x2M,x3V,x0T,x4V),reachableCast(x0T,x1V)),
    Rule(89	,Value(0.5, Token(89	)),ptsVT(x3V,x0T), McheckCastInst(x2M,x3V,x0T,x4V),notSub(x0T,x1T)),
    Rule(90	,Value(0.5, Token(90	)),ptsVT(x3V,x1T), McheckCastInst(x2M,x3V,x0T,x4V),notSub(x0T,x1T)),
    Rule(91	,Value(0.5, Token(91	)),ptsVT(x4V,x1T), McheckCastInst(x2M,x3V,x0T,x4V),notSub(x0T,x1T)),
    Rule(92	,Value(0.5, Token(92	)),ptsVT(x3V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),badCast(x0V,x1T)),
    Rule(93	,Value(0.5, Token(93	)),ptsVT(x3V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x0V,x1T)),
    Rule(94	,Value(0.5, Token(94	)),ptsVT(x3V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x0V,x1T)),
    Rule(95	,Value(0.5, Token(95	)),ptsVT(x3V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),notSub(x0T,x1T)),
    Rule(96	,Value(0.5, Token(96	)),ptsVT(x3V,x1T), HT(x0H,x1T),McheckCastInst(x2M,x3V,x1T,x4V)),
    Rule(97	,Value(0.5, Token(97	)),ptsVT(x2V,x0T), badCast(x2V,x0T),reachableCast(x0T,x1V)),
    Rule(98	,Value(0.5, Token(98	)),ptsVT(x2V,x0T), reachableCast(x0T,x1V),unsafeDowncast(x2V,x0T)),
    Rule(99	,Value(0.5, Token(99	)),ptsVT(x2V,x0T), badCast(x2V,x0T),notSub(x0T,x1T)),
    Rule(100	,Value(0.5, Token(100	)),ptsVT(x2V,x0T), notSub(x0T,x1T),unsafeDowncast(x2V,x0T)),
    Rule(101	,Value(0.5, Token(101	)),ptsVT(x2V,x1T), badCast(x2V,x0T),notSub(x0T,x1T)),
    Rule(102	,Value(0.5, Token(102	)),ptsVT(x2V,x1T), notSub(x0T,x1T),ptsVT(x2V,x0T)),
    Rule(103	,Value(0.5, Token(103	)),ptsVT(x2V,x1T), notSub(x0T,x1T),unsafeDowncast(x2V,x0T)),
    Rule(104	,Value(0.5, Token(104	)),ptsVT(x2V,x1T), HT(x0H,x1T),VH(x2V,x0H)),
    Rule(105	,Value(0.5, Token(105	)),ptsVT(x2V,x0T), badCast(x2V,x1T),notSub(x0T,x1T)),
    Rule(106	,Value(0.5, Token(106	)),ptsVT(x2V,x0T), notSub(x0T,x1T),ptsVT(x2V,x1T)),
    Rule(107	,Value(0.5, Token(107	)),ptsVT(x2V,x0T), notSub(x0T,x1T),unsafeDowncast(x2V,x1T)),
    Rule(108	,Value(0.5, Token(108	)),ptsVT(x2V,x1T), badCast(x0V,x1T),unsafeDowncast(x2V,x1T)),
    Rule(109	,Value(0.5, Token(109	)),ptsVT(x2V,x1T), badCast(x2V,x1T),ptsVT(x0V,x1T)),
    Rule(110	,Value(0.5, Token(110	)),ptsVT(x2V,x1T), ptsVT(x0V,x1T),unsafeDowncast(x2V,x1T)),
    Rule(111	,Value(0.5, Token(111	)),ptsVT(x2V,x1T), badCast(x2V,x1T),unsafeDowncast(x0V,x1T)),
    Rule(130	,Value(0.5, Token(130	)),ptsVT(x0V,x4T), McheckCastInst(x2M,x3V,x4T,x0V),unsafeDowncast(x0V,x1T)),
    Rule(131	,Value(0.5, Token(131	)),ptsVT(x0V,x4T), McheckCastInst(x2M,x3V,x4T,x0V),VH(x0V,x1H)),
    Rule(132	,Value(0.5, Token(132	)),ptsVT(x0V,x2T), badCast(x0V,x1T),reachableCast(x2T,x0V)),
    Rule(133	,Value(0.5, Token(133	)),ptsVT(x0V,x2T), ptsVT(x0V,x1T),reachableCast(x2T,x0V)),
    Rule(134	,Value(0.5, Token(134	)),ptsVT(x0V,x2T), reachableCast(x2T,x0V),unsafeDowncast(x0V,x1T)),
    Rule(135	,Value(0.5, Token(135	)),ptsVT(x0V,x2T), VH(x0V,x1H),reachableCast(x2T,x0V)),
    Rule(136	,Value(0.5, Token(136	)),ptsVT(x1V,x4T), McheckCastInst(x0M,x1V,x2T,x3V),badCast(x1V,x4T)),
    Rule(137	,Value(0.5, Token(137	)),ptsVT(x1V,x4T), McheckCastInst(x0M,x1V,x2T,x3V),unsafeDowncast(x1V,x4T)),
    Rule(138	,Value(0.5, Token(138	)),ptsVT(x3V,x4T), McheckCastInst(x0M,x1V,x2T,x3V),badCast(x1V,x4T)),
    Rule(139	,Value(0.5, Token(139	)),ptsVT(x3V,x4T), McheckCastInst(x0M,x1V,x2T,x3V),ptsVT(x1V,x4T)),
    Rule(140	,Value(0.5, Token(140	)),ptsVT(x3V,x4T), McheckCastInst(x0M,x1V,x2T,x3V),unsafeDowncast(x1V,x4T)),
    Rule(141	,Value(0.5, Token(141	)),ptsVT(x4V,x2T), McheckCastInst(x0M,x1V,x2T,x3V),badCast(x4V,x2T)),
    Rule(142	,Value(0.5, Token(142	)),ptsVT(x4V,x2T), McheckCastInst(x0M,x1V,x2T,x3V),unsafeDowncast(x4V,x2T)),
    Rule(143	,Value(0.5, Token(143	)),ptsVT(x1V,x4T), McheckCastInst(x0M,x1V,x2T,x3V),badCast(x3V,x4T)),
    Rule(144	,Value(0.5, Token(144	)),ptsVT(x1V,x4T), McheckCastInst(x0M,x1V,x2T,x3V),ptsVT(x3V,x4T)),
    Rule(145	,Value(0.5, Token(145	)),ptsVT(x1V,x4T), McheckCastInst(x0M,x1V,x2T,x3V),unsafeDowncast(x3V,x4T)),
    Rule(146	,Value(0.5, Token(146	)),ptsVT(x3V,x4T), McheckCastInst(x0M,x1V,x2T,x3V),badCast(x3V,x4T)),
    Rule(147	,Value(0.5, Token(147	)),ptsVT(x3V,x4T), McheckCastInst(x0M,x1V,x2T,x3V),unsafeDowncast(x3V,x4T)),
    Rule(148	,Value(0.5, Token(148	)),ptsVT(x1V,x2T), McheckCastInst(x0M,x1V,x2T,x3V),reachableCast(x4T,x1V)),
    Rule(149	,Value(0.5, Token(149	)),ptsVT(x0V,x1T), badCast(x0V,x1T),reachableCast(x2T,x0V)),
    Rule(150	,Value(0.5, Token(150	)),ptsVT(x0V,x1T), reachableCast(x2T,x0V),unsafeDowncast(x0V,x1T)),
    Rule(151	,Value(0.5, Token(151	)),ptsVT(x3V,x0T), McheckCastInst(x2M,x3V,x0T,x4V),reachableCast(x0T,x4V)),
    Rule(152	,Value(0.5, Token(152	)),ptsVT(x4V,x0T), McheckCastInst(x2M,x3V,x0T,x4V),reachableCast(x0T,x3V)),
    Rule(153	,Value(0.5, Token(153	)),ptsVT(x3V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),badCast(x3V,x1T)),
    Rule(154	,Value(0.5, Token(154	)),ptsVT(x3V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x3V,x1T)),
    Rule(155	,Value(0.5, Token(155	)),ptsVT(x3V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),badCast(x4V,x1T)),
    Rule(156	,Value(0.5, Token(156	)),ptsVT(x3V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x4V,x1T)),
    Rule(157	,Value(0.5, Token(157	)),ptsVT(x3V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x4V,x1T)),
    Rule(158	,Value(0.5, Token(158	)),ptsVT(x4V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),badCast(x3V,x1T)),
    Rule(159	,Value(0.5, Token(159	)),ptsVT(x4V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x3V,x1T)),
    Rule(160	,Value(0.5, Token(160	)),ptsVT(x4V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x3V,x1T)),
    Rule(166	,Value(0.5, Token(166	)),reachableCast(x2T,x3V), McheckCastInst(x0M,x1V,x2T,x3V)),
    Rule(189	,Value(0.5, Token(189	)),reachableCast(x1T,x2V), notSub(x0T,x1T),unsafeDowncast(x2V,x0T)),
    Rule(190	,Value(0.5, Token(190	)),reachableCast(x1T,x2V), HT(x0H,x1T),VH(x2V,x0H)),
    Rule(191	,Value(0.5, Token(191	)),reachableCast(x0T,x2V), badCast(x2V,x1T),notSub(x0T,x1T)),
    Rule(192	,Value(0.5, Token(192	)),reachableCast(x0T,x2V), notSub(x0T,x1T),ptsVT(x2V,x1T)),
    Rule(193	,Value(0.5, Token(193	)),reachableCast(x0T,x2V), notSub(x0T,x1T),unsafeDowncast(x2V,x1T)),
    Rule(194	,Value(0.5, Token(194	)),reachableCast(x1T,x2V), badCast(x0V,x1T),ptsVT(x2V,x1T)),
    Rule(195	,Value(0.5, Token(195	)),reachableCast(x1T,x2V), badCast(x0V,x1T),unsafeDowncast(x2V,x1T)),
    Rule(196	,Value(0.5, Token(196	)),reachableCast(x1T,x2V), badCast(x2V,x1T),ptsVT(x0V,x1T)),
    Rule(197	,Value(0.5, Token(197	)),reachableCast(x1T,x2V), ptsVT(x0V,x1T),unsafeDowncast(x2V,x1T)),
    Rule(198	,Value(0.5, Token(198	)),reachableCast(x1T,x2V), badCast(x2V,x1T),unsafeDowncast(x0V,x1T)),
    Rule(199	,Value(0.5, Token(199	)),reachableCast(x1T,x2V), ptsVT(x2V,x1T),unsafeDowncast(x0V,x1T)),
    Rule(200	,Value(0.5, Token(200	)),reachableCast(x1T,x2V), badCast(x2V,x1T),notSub(x0T,x1T)),
    Rule(201	,Value(0.5, Token(201	)),reachableCast(x1T,x2V), notSub(x0T,x1T),ptsVT(x2V,x1T)),
    Rule(202	,Value(0.5, Token(202	)),reachableCast(x1T,x2V), notSub(x0T,x1T),unsafeDowncast(x2V,x1T)),
    Rule(203	,Value(0.5, Token(203	)),reachableCast(x1T,x2V), HT(x0H,x1T),badCast(x2V,x1T)),
    Rule(204	,Value(0.5, Token(204	)),reachableCast(x1T,x2V), HT(x0H,x1T),ptsVT(x2V,x1T)),
    Rule(205	,Value(0.5, Token(205	)),reachableCast(x1T,x2V), HT(x0H,x1T),unsafeDowncast(x2V,x1T)),
    Rule(206	,Value(0.5, Token(206	)),reachableCast(x1T,x2V), notSub(x0T,x1T),reachableCast(x0T,x2V)),
    Rule(207	,Value(0.5, Token(207	)),reachableCast(x0T,x2V), notSub(x0T,x1T),reachableCast(x1T,x2V)),
    Rule(208	,Value(0.5, Token(208	)),reachableCast(x4T,x0V), McheckCastInst(x2M,x3V,x4T,x0V),badCast(x0V,x1T)),
    Rule(209	,Value(0.5, Token(209	)),reachableCast(x4T,x0V), McheckCastInst(x2M,x3V,x4T,x0V),ptsVT(x0V,x1T)),
    Rule(299	,Value(0.5, Token(299	)),unsafeDowncast(x3V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),badCast(x4V,x1T)),
    Rule(300	,Value(0.5, Token(300	)),unsafeDowncast(x3V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x4V,x1T)),
    Rule(301	,Value(0.5, Token(301	)),unsafeDowncast(x3V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x4V,x1T)),
    Rule(302	,Value(0.5, Token(302	)),unsafeDowncast(x4V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),badCast(x3V,x1T)),
    Rule(303	,Value(0.5, Token(303	)),unsafeDowncast(x4V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x3V,x1T)),
    Rule(304	,Value(0.5, Token(304	)),unsafeDowncast(x4V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),unsafeDowncast(x3V,x1T)),
    Rule(305	,Value(0.5, Token(305	)),unsafeDowncast(x4V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),badCast(x4V,x1T)),
    Rule(306	,Value(0.5, Token(306	)),unsafeDowncast(x4V,x1T), McheckCastInst(x2M,x3V,x1T,x4V),ptsVT(x4V,x1T)),
    Rule(307	,Value(0.5, Token(307	)),unsafeDowncast(x2V,x0T), badCast(x2V,x0T),reachableCast(x0T,x2V)),
    Rule(308	,Value(0.5, Token(308	)),unsafeDowncast(x2V,x0T), ptsVT(x2V,x0T),reachableCast(x0T,x2V)),
  )

  override val expected: Set[Any] = Set(20, 104, 166, 307)
  override val maxVarCount: Int = 4
}
