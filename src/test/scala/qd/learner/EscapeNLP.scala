package qd
package learner

import org.scalatest.{FunSuite, Ignore}

class EscapeNLP extends FunSuite {
  val methodSet : Set[Atom] = Range(0, 3).map(i => Atom(i)).toSet
  val method : Domain = Domain("Method", methodSet)

  val variableSet : Set[Atom] = Range(0, 4).map(i => Atom(i)).toSet
  val variable : Domain = Domain("Variable", variableSet)

  val heapSet : Set[Atom] = Range(0, 5).map(i => Atom(i)).toSet
  val heap : Domain = Domain("Heap", heapSet)

  val MmethArg : Relation = Relation("MmethArg", method, variable)
  val MmethRet : Relation = Relation("MmethRet", method, variable)
  val VH : Relation = Relation("VH", variable, heap)
  val HFH : Relation = Relation("HFH", heap, heap)
  val rMH : Relation = Relation("rMH", method, heap)
  val rRH : Relation = Relation("rRH", method, heap)
  val rHH : Relation = Relation("rHH", heap, heap)

  val MmethArgTuples : Set[DTuple] = Set((0,0),(1,3),(2,2)).map{ case (a,b) => DTuple(Atom(a), Atom(b)) }
  val MmethRetTuples : Set[DTuple] = Set((0,1),(2,1)).map{ case (a,b) => DTuple(Atom(a), Atom(b)) }
  val VHTuples : Set[DTuple] = Set((1,1),(0,0),(2,2),(3,3)).map{ case (a,b) => DTuple(Atom(a), Atom(b)) }
  val HFHTuples : Set[DTuple] = Set((0,1),(1,2),(2,3),(0,2)).map{ case (a,b) => DTuple(Atom(a), Atom(b)) }
  val rMHTuples : Set[DTuple] = Set((0,0),(0,1),(0,2),(0,3),(1,3),(2,2),(2,3)).map{ case (a,b) => DTuple(Atom(a), Atom(b)) }
  val rRHTuples : Set[DTuple] = Set((0,1),(0,2),(0,3),(2,1),(2,2),(2,3)).map{ case (a,b) => DTuple(Atom(a), Atom(b)) }
  val rHHTuples : Set[DTuple] = Set((0,1),(1,2),(2,3),(0,2),(1,3),(0,3)).map{ case (a,b) => DTuple(Atom(a), Atom(b)) }

  val edb : Config = Config(
    MmethArg -> (Instance(MmethArg) ++ MmethArgTuples.map(t => t -> One).toMap),
    MmethRet -> (Instance(MmethRet) ++ MmethRetTuples.map(t => t -> One).toMap),
    VH -> (Instance(VH) ++ VHTuples.map(t => t -> One).toMap),
    HFH -> (Instance(HFH) ++ HFHTuples.map(t => t -> One).toMap)
  )

  val refOut : Config = Config (
    rMH -> (Instance(rMH) ++ rMHTuples.map(t => t -> One).toMap),
    rRH -> (Instance(rRH) ++ rRHTuples.map(t => t -> One).toMap),
    rHH -> (Instance(rHH) ++ rHHTuples.map(t => t -> One).toMap)
  )

  val x0H : Variable = Variable("x0H", heap)
  val x1H : Variable = Variable("x1H", heap)
  val x2H : Variable = Variable("x2H", heap)
  val x3H : Variable = Variable("x3H", heap)

  val x0V : Variable = Variable("x0V", variable)
  val x1V : Variable = Variable("x1V", variable)
  val x2V : Variable = Variable("x2V", variable)
  val x3V : Variable = Variable("x3V", variable)

  val x0M : Variable = Variable("x0M", method)
  val x1M : Variable = Variable("x1M", method)
  val x2M : Variable = Variable("x2M", method)
  val x3M : Variable = Variable("x3M", method)

  // result/escape_253.nlp
  val soup1: Set[Rule] = Set(
    Rule(1, Value(0.249, Token(1)), rMH(x1M, x0H),HFH(x1H, x0H)),
    Rule(2, Value(0.237, Token(2)), rMH(x1M, x0H),HFH(x1H, x0H)),
    Rule(3, Value(0.177, Token(3)), rRH(x1M, x0H),HFH(x1H, x0H)),
    Rule(4, Value(0.168, Token(4)), rRH(x1M, x0H),HFH(x1H, x0H)),
    Rule(5, Value(0.132, Token(5)), rHH(x1H, x0H),HFH(x1H, x0H)),
    Rule(6, Value(0.105, Token(6)), rHH(x1H, x0H),HFH(x1H, x0H)),
    Rule(7, Value(0.092, Token(7)), rHH(x1H, x0H),MmethRet(x1M, x0V)),
    Rule(8, Value(0.085, Token(8)), rRH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(9, Value(0.084, Token(9)), rMH(x1M, x0H),rRH(x1M, x0H)),
    Rule(10, Value(0.076, Token(10)), rHH(x1H, x0H),MmethArg(x1M, x0V)),
    Rule(11, Value(0.073, Token(11)), rHH(x1H, x0H),rMH(x1M, x0H)),
    Rule(12, Value(0.070, Token(12)), rHH(x1H, x0H),rRH(x1M, x0H)),
    Rule(13, Value(0.066, Token(13)), rRH(x1M, x0H),rRH(x1M, x0H)),
    Rule(14, Value(0.065, Token(14)), rHH(x1H, x0H),rHH(x1H, x0H)),
    Rule(15, Value(0.064, Token(15)), rRH(x1M, x0H),rMH(x1M, x0H)),
    Rule(16, Value(0.062, Token(16)), rRH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(17, Value(0.061, Token(17)), rRH(x1M, x0H),rHH(x1H, x0H)),
    Rule(18, Value(0.061, Token(18)), rMH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(19, Value(0.061, Token(19)), rMH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(20, Value(0.059, Token(20)), rMH(x2M, x0H),HFH(x1H, x0H), HFH(x2H, x1H)),
    Rule(21, Value(0.058, Token(21)), rMH(x1M, x0H),rHH(x1H, x0H)),
    Rule(22, Value(0.053, Token(22)), rMH(x1M, x0H),rMH(x1M, x0H)),
    Rule(23, Value(0.052, Token(23)), rMH(x1M, x0H),VH(x1V, x0H)),
    Rule(24, Value(0.051, Token(24)), rHH(x1H, x0H),MmethRet(x1M, x0V)),
    Rule(25, Value(0.045, Token(25)), rRH(x1M, x0H),VH(x1V, x0H)),
    Rule(26, Value(0.041, Token(26)), rHH(x1H, x0H),VH(x1V, x0H)),
    Rule(27, Value(0.040, Token(27)), rRH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(28, Value(0.035, Token(28)), rHH(x1H, x0H),MmethArg(x1M, x0V)),
    Rule(29, Value(0.033, Token(29)), rMH(x1M, x0H),rRH(x1M, x0H)),
    Rule(30, Value(0.032, Token(30)), rHH(x1H, x0H),rMH(x1M, x0H)),
  )
// result/escape_34.nlp
  val soup2: Set[Rule] = Set(
    Rule(1, Value(0.241, Token(1)), rRH(x1M, x0H),HFH(x1H, x0H)),
    Rule(2, Value(0.231, Token(2)), rRH(x1M, x0H),HFH(x1H, x0H)),
    Rule(3, Value(0.201, Token(3)), rMH(x1M, x0H),HFH(x1H, x0H)),
    Rule(4, Value(0.198, Token(4)), rMH(x1M, x0H),HFH(x1H, x0H)),
    Rule(5, Value(0.182, Token(5)), rHH(x1H, x0H),HFH(x1H, x0H)),
    Rule(6, Value(0.167, Token(6)), rHH(x1H, x0H),HFH(x1H, x0H)),
    Rule(7, Value(0.117, Token(7)), rHH(x1H, x0H),MmethRet(x1M, x0V)),
    Rule(8, Value(0.088, Token(8)), rRH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(9, Value(0.088, Token(9)), rHH(x1H, x0H),MmethArg(x1M, x0V)),
    Rule(10, Value(0.083, Token(10)), rMH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(11, Value(0.082, Token(11)), rRH(x1M, x0H),rHH(x1H, x0H)),
    Rule(12, Value(0.082, Token(12)), rMH(x1M, x0H),rHH(x1H, x0H)),
    Rule(13, Value(0.071, Token(13)), rRH(x1M, x0H),rMH(x1M, x0H)),
    Rule(14, Value(0.071, Token(14)), rHH(x1H, x0H),MmethRet(x1M, x0V)),
    Rule(15, Value(0.067, Token(15)), rMH(x1M, x0H),rRH(x1M, x0H)),
    Rule(16, Value(0.067, Token(16)), rMH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(17, Value(0.067, Token(17)), rHH(x1H, x0H),rRH(x1M, x0H)),
    Rule(18, Value(0.061, Token(18)), rRH(x1M, x0H),rRH(x1M, x0H)),
    Rule(19, Value(0.059, Token(19)), rRH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(20, Value(0.059, Token(20)), rHH(x1H, x0H),rHH(x1H, x0H)),
    Rule(21, Value(0.058, Token(21)), rMH(x1M, x0H),rMH(x1M, x0H)),
    Rule(22, Value(0.056, Token(22)), rRH(x2M, x0H),HFH(x1H, x0H), HFH(x2H, x1H)),
    Rule(23, Value(0.053, Token(23)), rHH(x1H, x0H),rMH(x1M, x0H)),
    Rule(24, Value(0.040, Token(24)), rMH(x2M, x0H),HFH(x1H, x0H), HFH(x2H, x1H)),
    Rule(25, Value(0.040, Token(25)), rHH(x1H, x0H),MmethArg(x1M, x0V)),
    Rule(26, Value(0.039, Token(26)), rRH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(27, Value(0.039, Token(27)), rMH(x1M, x0H),VH(x1V, x0H)),
    Rule(28, Value(0.038, Token(28)), rMH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(29, Value(0.037, Token(29)), rMH(x1M, x0H),rHH(x1H, x0H)),
    Rule(30, Value(0.036, Token(30)), rRH(x1M, x0H),VH(x1V, x0H)),
    Rule(31, Value(0.036, Token(31)), rRH(x1M, x0H),rHH(x1H, x0H)),
  )
// result/escape_3519.nlp
  val soup3: Set[Rule] = Set(
    Rule(1, Value(0.270, Token(1)), rHH(x1H, x0H),HFH(x1H, x0H)),
    Rule(2, Value(0.260, Token(2)), rRH(x1M, x0H),HFH(x1H, x0H)),
    Rule(3, Value(0.251, Token(3)), rHH(x1H, x0H),HFH(x1H, x0H)),
    Rule(4, Value(0.246, Token(4)), rRH(x1M, x0H),HFH(x1H, x0H)),
    Rule(5, Value(0.211, Token(5)), rMH(x1M, x0H),HFH(x1H, x0H)),
    Rule(6, Value(0.207, Token(6)), rMH(x1M, x0H),HFH(x1H, x0H)),
    Rule(7, Value(0.108, Token(7)), rHH(x1H, x0H),MmethArg(x1M, x0V)),
    Rule(8, Value(0.097, Token(8)), rHH(x1H, x0H),MmethRet(x1M, x0V)),
    Rule(9, Value(0.092, Token(9)), rMH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(10, Value(0.085, Token(10)), rMH(x1M, x0H),rHH(x1H, x0H)),
    Rule(11, Value(0.081, Token(11)), rRH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(12, Value(0.075, Token(12)), rRH(x1M, x0H),rRH(x1M, x0H)),
    Rule(13, Value(0.073, Token(13)), rMH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(14, Value(0.072, Token(14)), rMH(x1M, x0H),rRH(x1M, x0H)),
    Rule(15, Value(0.069, Token(15)), rHH(x2H, x0H),HFH(x1H, x0H), HFH(x2H, x1H)),
    Rule(16, Value(0.067, Token(16)), rHH(x1H, x0H),rRH(x1M, x0H)),
    Rule(17, Value(0.065, Token(17)), rRH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(18, Value(0.064, Token(18)), rRH(x2M, x0H),HFH(x1H, x0H), HFH(x2H, x1H)),
    Rule(19, Value(0.062, Token(19)), rRH(x1M, x0H),rHH(x1H, x0H)),
    Rule(20, Value(0.057, Token(20)), rHH(x1H, x0H),rMH(x1M, x0H)),
    Rule(21, Value(0.057, Token(21)), rHH(x1H, x0H),rHH(x1H, x0H)),
    Rule(22, Value(0.056, Token(22)), rRH(x1M, x0H),rMH(x1M, x0H)),
    Rule(23, Value(0.046, Token(23)), rHH(x1H, x0H),MmethArg(x1M, x0V)),
    Rule(24, Value(0.044, Token(24)), rMH(x2M, x0H),HFH(x1H, x0H), HFH(x2H, x1H)),
    Rule(25, Value(0.042, Token(25)), rMH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(26, Value(0.039, Token(26)), rMH(x1M, x0H),rMH(x1M, x0H)),
    Rule(27, Value(0.039, Token(27)), rMH(x1M, x0H),rHH(x1H, x0H)),
    Rule(28, Value(0.038, Token(28)), rHH(x1H, x0H),MmethRet(x1M, x0V)),
    Rule(29, Value(0.036, Token(29)), rMH(x1M, x0H),VH(x1V, x0H)),
    Rule(30, Value(0.032, Token(30)), rRH(x1M, x0H),VH(x1V, x0H)),
  )
// result/escape_42.nlp
  val soup4: Set[Rule] = Set(
    Rule(1, Value(0.389, Token(1)), rRH(x1M, x0H),HFH(x1H, x0H)),
    Rule(2, Value(0.378, Token(2)), rHH(x1H, x0H),HFH(x1H, x0H)),
    Rule(3, Value(0.363, Token(3)), rRH(x1M, x0H),HFH(x1H, x0H)),
    Rule(4, Value(0.342, Token(4)), rHH(x1H, x0H),HFH(x1H, x0H)),
    Rule(5, Value(0.158, Token(5)), rMH(x1M, x0H),HFH(x1H, x0H)),
    Rule(6, Value(0.145, Token(6)), rMH(x1M, x0H),HFH(x1H, x0H)),
    Rule(7, Value(0.141, Token(7)), rRH(x2M, x0H),HFH(x1H, x0H), HFH(x2H, x1H)),
    Rule(8, Value(0.130, Token(8)), rHH(x2H, x0H),HFH(x1H, x0H), HFH(x2H, x1H)),
    Rule(9, Value(0.120, Token(9)), rMH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(10, Value(0.089, Token(10)), rRH(x1M, x0H),rMH(x1M, x0H)),
    Rule(11, Value(0.088, Token(11)), rMH(x1M, x0H),rMH(x1M, x0H)),
    Rule(12, Value(0.083, Token(12)), rMH(x1M, x0H),rRH(x1M, x0H)),
    Rule(13, Value(0.083, Token(13)), rMH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(14, Value(0.076, Token(14)), rHH(x1H, x0H),MmethArg(x1M, x0V)),
    Rule(15, Value(0.074, Token(15)), rHH(x1H, x0H),rRH(x1M, x0H)),
    Rule(16, Value(0.074, Token(16)), rHH(x1H, x0H),rMH(x1M, x0H)),
    Rule(17, Value(0.072, Token(17)), rMH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(18, Value(0.064, Token(18)), rRH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(19, Value(0.063, Token(19)), rHH(x1H, x0H),MmethRet(x1M, x0V)),
    Rule(20, Value(0.061, Token(20)), rMH(x1M, x0H),rHH(x1H, x0H)),
    Rule(21, Value(0.060, Token(21)), rRH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(22, Value(0.057, Token(22)), rRH(x1M, x0H),rRH(x1M, x0H)),
    Rule(23, Value(0.047, Token(23)), rHH(x1H, x0H),rHH(x1H, x0H)),
    Rule(24, Value(0.045, Token(24)), rMH(x1M, x0H),rMH(x1M, x0H)),
    Rule(25, Value(0.044, Token(25)), rRH(x1M, x0H),rHH(x1H, x0H)),
    Rule(26, Value(0.041, Token(26)), rMH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(27, Value(0.038, Token(27)), rMH(x1M, x0H),rRH(x1M, x0H)),
    Rule(28, Value(0.035, Token(28)), rRH(x2M, x0H),rMH(x1M, x0H), HFH(x2H, x1H)),
    Rule(29, Value(0.035, Token(29)), rRH(x1M, x0H),VH(x1V, x0H)),
  )
// result/escape_481.nlp
  val soup5: Set[Rule] = Set(
    Rule(1, Value(0.301, Token(1)), rHH(x1H, x0H),HFH(x1H, x0H)),
    Rule(2, Value(0.277, Token(2)), rHH(x1H, x0H),HFH(x1H, x0H)),
    Rule(3, Value(0.217, Token(3)), rMH(x1M, x0H),HFH(x1H, x0H)),
    Rule(4, Value(0.213, Token(4)), rMH(x1M, x0H),HFH(x1H, x0H)),
    Rule(5, Value(0.186, Token(5)), rRH(x1M, x0H),HFH(x1H, x0H)),
    Rule(6, Value(0.180, Token(6)), rRH(x1M, x0H),HFH(x1H, x0H)),
    Rule(7, Value(0.102, Token(7)), rRH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(8, Value(0.088, Token(8)), rMH(x1M, x0H),rHH(x1H, x0H)),
    Rule(9, Value(0.085, Token(9)), rHH(x2H, x0H),HFH(x1H, x0H), HFH(x2H, x1H)),
    Rule(10, Value(0.078, Token(10)), rMH(x1M, x0H),rRH(x1M, x0H)),
    Rule(11, Value(0.078, Token(11)), rMH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(12, Value(0.074, Token(12)), rMH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(13, Value(0.071, Token(13)), rRH(x1M, x0H),rHH(x1H, x0H)),
    Rule(14, Value(0.071, Token(14)), rHH(x1H, x0H),rHH(x1H, x0H)),
    Rule(15, Value(0.069, Token(15)), rMH(x1M, x0H),rMH(x1M, x0H)),
    Rule(16, Value(0.069, Token(16)), rHH(x1H, x0H),MmethRet(x1M, x0V)),
    Rule(17, Value(0.068, Token(17)), rHH(x1H, x0H),rMH(x1M, x0H)),
    Rule(18, Value(0.065, Token(18)), rRH(x1M, x0H),rRH(x1M, x0H)),
    Rule(19, Value(0.063, Token(19)), rRH(x1M, x0H),rMH(x1M, x0H)),
    Rule(20, Value(0.063, Token(20)), rRH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(21, Value(0.063, Token(21)), rHH(x1H, x0H),MmethArg(x1M, x0V)),
    Rule(22, Value(0.060, Token(22)), rHH(x1H, x0H),rRH(x1M, x0H)),
    Rule(23, Value(0.058, Token(23)), rRH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(24, Value(0.047, Token(24)), rMH(x2M, x0H),HFH(x1H, x0H), HFH(x2H, x1H)),
    Rule(25, Value(0.043, Token(25)), rRH(x1M, x0H),VH(x1V, x0H)),
    Rule(26, Value(0.042, Token(26)), rMH(x1M, x0H),rHH(x1H, x0H)),
    Rule(27, Value(0.038, Token(27)), rMH(x1M, x0H),VH(x1V, x0H)),
    Rule(28, Value(0.038, Token(28)), rHH(x1H, x0H),VH(x1V, x0H)),
    Rule(29, Value(0.034, Token(29)), rRH(x2M, x0H),HFH(x1H, x0H), HFH(x2H, x1H)),
    Rule(30, Value(0.033, Token(30)), rMH(x1M, x0H),MmethRet(x1M, x0V)),
  )
// result/escape_499.nlp
  val soup6: Set[Rule] = Set(
    Rule(1, Value(0.416, Token(1)), rHH(x1H, x0H),HFH(x1H, x0H)),
    Rule(2, Value(0.406, Token(2)), rHH(x1H, x0H),HFH(x1H, x0H)),
    Rule(3, Value(0.179, Token(3)), rRH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(4, Value(0.169, Token(4)), rHH(x2H, x0H),HFH(x1H, x0H), HFH(x2H, x1H)),
    Rule(5, Value(0.163, Token(5)), rRH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(6, Value(0.160, Token(6)), rMH(x1M, x0H),HFH(x1H, x0H)),
    Rule(7, Value(0.136, Token(7)), rMH(x1M, x0H),HFH(x1H, x0H)),
    Rule(8, Value(0.131, Token(8)), rMH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(9, Value(0.127, Token(9)), rRH(x1M, x0H),HFH(x1H, x0H)),
    Rule(10, Value(0.127, Token(10)), rMH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(11, Value(0.089, Token(11)), rRH(x1M, x0H),HFH(x1H, x0H)),
    Rule(12, Value(0.087, Token(12)), rMH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(13, Value(0.083, Token(13)), rRH(x1M, x0H),rRH(x1M, x0H)),
    Rule(14, Value(0.082, Token(14)), rMH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(15, Value(0.078, Token(15)), rRH(x1M, x0H),rMH(x1M, x0H)),
    Rule(16, Value(0.067, Token(16)), rRH(x1M, x0H),rHH(x1H, x0H)),
    Rule(17, Value(0.067, Token(17)), rMH(x1M, x0H),rRH(x1M, x0H)),
    Rule(18, Value(0.062, Token(18)), rRH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(19, Value(0.061, Token(19)), rHH(x1H, x0H),rHH(x1H, x0H)),
    Rule(20, Value(0.058, Token(20)), rHH(x1H, x0H),rRH(x1M, x0H)),
    Rule(21, Value(0.057, Token(21)), rMH(x1M, x0H),rHH(x1H, x0H)),
    Rule(22, Value(0.057, Token(22)), rHH(x1H, x0H),MmethArg(x1M, x0V)),
    Rule(23, Value(0.055, Token(23)), rMH(x1M, x0H),rMH(x1M, x0H)),
    Rule(24, Value(0.043, Token(24)), rHH(x1H, x0H),rMH(x1M, x0H)),
    Rule(25, Value(0.042, Token(25)), rHH(x1H, x0H),MmethRet(x1M, x0V)),
    Rule(26, Value(0.039, Token(26)), rRH(x1M, x0H),VH(x1V, x0H)),
    Rule(27, Value(0.037, Token(27)), rMH(x1M, x0H),VH(x1V, x0H)),
    Rule(28, Value(0.036, Token(28)), rRH(x1M, x0H),rRH(x1M, x0H)),
    Rule(29, Value(0.033, Token(29)), rRH(x1M, x0H),rMH(x1M, x0H)),
  )
// result/escape_591.nlp
  val soup7: Set[Rule] = Set(
    Rule(1, Value(0.264, Token(1)), rHH(x1H, x0H),HFH(x1H, x0H)),
    Rule(2, Value(0.247, Token(2)), rHH(x1H, x0H),HFH(x1H, x0H)),
    Rule(3, Value(0.179, Token(3)), rRH(x1M, x0H),HFH(x1H, x0H)),
    Rule(4, Value(0.168, Token(4)), rMH(x1M, x0H),HFH(x1H, x0H)),
    Rule(5, Value(0.162, Token(5)), rRH(x1M, x0H),HFH(x1H, x0H)),
    Rule(6, Value(0.157, Token(6)), rMH(x1M, x0H),HFH(x1H, x0H)),
    Rule(7, Value(0.135, Token(7)), rRH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(8, Value(0.106, Token(8)), rHH(x1H, x0H),rHH(x1H, x0H)),
    Rule(9, Value(0.096, Token(9)), rRH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(10, Value(0.094, Token(10)), rMH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(11, Value(0.092, Token(11)), rHH(x1H, x0H),rMH(x1M, x0H)),
    Rule(12, Value(0.077, Token(12)), rMH(x1M, x0H),rHH(x1H, x0H)),
    Rule(13, Value(0.075, Token(13)), rRH(x1M, x0H),rHH(x1H, x0H)),
    Rule(14, Value(0.071, Token(14)), rMH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(15, Value(0.070, Token(15)), rRH(x1M, x0H),rMH(x1M, x0H)),
    Rule(16, Value(0.066, Token(16)), rMH(x1M, x0H),rRH(x1M, x0H)),
    Rule(17, Value(0.064, Token(17)), rMH(x1M, x0H),rMH(x1M, x0H)),
    Rule(18, Value(0.066, Token(18)), rHH(x1H, x0H),MmethRet(x1M, x0V)),
    Rule(19, Value(0.065, Token(19)), rHH(x2H, x0H),HFH(x1H, x0H), HFH(x2H, x1H)),
    Rule(20, Value(0.062, Token(20)), rHH(x1H, x0H),rRH(x1M, x0H)),
    Rule(21, Value(0.061, Token(21)), rRH(x1M, x0H),rRH(x1M, x0H)),
    Rule(22, Value(0.053, Token(22)), rHH(x1H, x0H),MmethArg(x1M, x0V)),
    Rule(23, Value(0.052, Token(23)), rRH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(24, Value(0.048, Token(24)), rMH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(25, Value(0.047, Token(25)), rHH(x1H, x0H),rHH(x1H, x0H)),
    Rule(26, Value(0.045, Token(26)), rRH(x1M, x0H),VH(x1V, x0H)),
    Rule(27, Value(0.040, Token(27)), rMH(x1M, x0H),VH(x1V, x0H)),
    Rule(28, Value(0.036, Token(28)), rHH(x1H, x0H),rMH(x1M, x0H)),
    Rule(29, Value(0.034, Token(29)), rMH(x1M, x0H),rHH(x1H, x0H)),
    Rule(30, Value(0.032, Token(30)), rRH(x1M, x0H),rHH(x1H, x0H)),
  )
// result/escape_6012.nlp
  val soup8: Set[Rule] = Set(
    Rule(1, Value(0.268, Token(1)), rRH(x1M, x0H),HFH(x1H, x0H)),
    Rule(2, Value(0.269, Token(2)), rHH(x1H, x0H),HFH(x1H, x0H)),
    Rule(3, Value(0.258, Token(3)), rHH(x1H, x0H),HFH(x1H, x0H)),
    Rule(4, Value(0.251, Token(4)), rRH(x1M, x0H),HFH(x1H, x0H)),
    Rule(5, Value(0.141, Token(5)), rMH(x1M, x0H),HFH(x1H, x0H)),
    Rule(6, Value(0.140, Token(6)), rMH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(7, Value(0.135, Token(7)), rMH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(8, Value(0.109, Token(8)), rMH(x1M, x0H),HFH(x1H, x0H)),
    Rule(9, Value(0.102, Token(9)), rMH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(10, Value(0.095, Token(10)), rRH(x1M, x0H),rRH(x1M, x0H)),
    Rule(11, Value(0.085, Token(11)), rMH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(12, Value(0.084, Token(12)), rHH(x1H, x0H),rHH(x1H, x0H)),
    Rule(13, Value(0.080, Token(13)), rMH(x1M, x0H),rHH(x1H, x0H)),
    Rule(14, Value(0.080, Token(14)), rHH(x1H, x0H),MmethArg(x1M, x0V)),
    Rule(15, Value(0.072, Token(15)), rRH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(16, Value(0.070, Token(16)), rHH(x2H, x0H),HFH(x1H, x0H), HFH(x2H, x1H)),
    Rule(17, Value(0.068, Token(17)), rRH(x2M, x0H),HFH(x1H, x0H), HFH(x2H, x1H)),
    Rule(18, Value(0.067, Token(18)), rMH(x1M, x0H),rRH(x1M, x0H)),
    Rule(19, Value(0.066, Token(19)), rHH(x1H, x0H),rRH(x1M, x0H)),
    Rule(20, Value(0.060, Token(20)), rRH(x1M, x0H),rMH(x1M, x0H)),
    Rule(21, Value(0.059, Token(21)), rRH(x1M, x0H),rHH(x1H, x0H)),
    Rule(22, Value(0.059, Token(22)), rRH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(23, Value(0.059, Token(23)), rHH(x1H, x0H),MmethRet(x1M, x0V)),
    Rule(24, Value(0.050, Token(24)), rMH(x1M, x0H),rMH(x1M, x0H)),
    Rule(25, Value(0.048, Token(25)), rRH(x1M, x0H),VH(x1V, x0H)),
    Rule(26, Value(0.045, Token(26)), rMH(x1M, x0H),VH(x1V, x0H)),
    Rule(27, Value(0.041, Token(27)), rRH(x1M, x0H),rRH(x1M, x0H)),
    Rule(28, Value(0.041, Token(28)), rHH(x1H, x0H),rMH(x1M, x0H)),
    Rule(29, Value(0.035, Token(29)), rMH(x1M, x0H),rHH(x1H, x0H)),
  )
// result/escape_68.nlp
  val soup9: Set[Rule] = Set(
    Rule(1, Value(0.321, Token(1)), rHH(x1H, x0H),HFH(x1H, x0H)),
    Rule(2, Value(0.288, Token(2)), rHH(x1H, x0H),HFH(x1H, x0H)),
    Rule(3, Value(0.206, Token(3)), rMH(x1M, x0H),HFH(x1H, x0H)),
    Rule(4, Value(0.202, Token(4)), rMH(x1M, x0H),HFH(x1H, x0H)),
    Rule(5, Value(0.183, Token(5)), rRH(x1M, x0H),HFH(x1H, x0H)),
    Rule(6, Value(0.173, Token(6)), rRH(x1M, x0H),HFH(x1H, x0H)),
    Rule(7, Value(0.099, Token(7)), rRH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(8, Value(0.097, Token(8)), rHH(x1H, x0H),rHH(x1H, x0H)),
    Rule(9, Value(0.095, Token(9)), rMH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(10, Value(0.094, Token(10)), rHH(x2H, x0H),HFH(x1H, x0H), HFH(x2H, x1H)),
    Rule(11, Value(0.086, Token(11)), rMH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(12, Value(0.084, Token(12)), rMH(x1M, x0H),rRH(x1M, x0H)),
    Rule(13, Value(0.083, Token(13)), rRH(x1M, x0H),rHH(x1H, x0H)),
    Rule(14, Value(0.079, Token(14)), rRH(x1M, x0H),rRH(x1M, x0H)),
    Rule(15, Value(0.071, Token(15)), rMH(x1M, x0H),rHH(x1H, x0H)),
    Rule(16, Value(0.070, Token(16)), rHH(x1H, x0H),rRH(x1M, x0H)),
    Rule(17, Value(0.063, Token(17)), rHH(x1H, x0H),MmethRet(x1M, x0V)),
    Rule(18, Value(0.060, Token(18)), rHH(x1H, x0H),MmethArg(x1M, x0V)),
    Rule(19, Value(0.057, Token(19)), rMH(x1M, x0H),VH(x1V, x0H)),
    Rule(20, Value(0.055, Token(20)), rRH(x1M, x0H),rMH(x1M, x0H)),
    Rule(21, Value(0.054, Token(21)), rRH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(22, Value(0.050, Token(22)), rMH(x1M, x0H),rMH(x1M, x0H)),
    Rule(23, Value(0.049, Token(23)), rHH(x1H, x0H),rMH(x1M, x0H)),
    Rule(24, Value(0.045, Token(24)), rMH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(25, Value(0.043, Token(25)), rRH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(26, Value(0.042, Token(26)), rMH(x2M, x0H),HFH(x1H, x0H), HFH(x2H, x1H)),
    Rule(27, Value(0.040, Token(27)), rMH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(28, Value(0.040, Token(28)), rHH(x1H, x0H),VH(x1V, x0H)),
    Rule(29, Value(0.039, Token(29)), rMH(x1M, x0H),rRH(x1M, x0H)),
    Rule(30, Value(0.038, Token(30)), rRH(x1M, x0H),VH(x1V, x0H)),
    Rule(31, Value(0.038, Token(31)), rRH(x1M, x0H),rHH(x1H, x0H)),
    Rule(32, Value(0.038, Token(32)), rHH(x1H, x0H),rHH(x1H, x0H)),
    Rule(33, Value(0.035, Token(33)), rRH(x1M, x0H),rRH(x1M, x0H)),
    Rule(34, Value(0.032, Token(34)), rRH(x2M, x0H),HFH(x1H, x0H), HFH(x2H, x1H)),
  )
// result/escape_991.nlp
  val soup10: Set[Rule] = Set(
    Rule(1, Value(0.278, Token(1)), rHH(x1H, x0H),HFH(x1H, x0H)),
    Rule(2, Value(0.258, Token(2)), rRH(x1M, x0H),HFH(x1H, x0H)),
    Rule(3, Value(0.248, Token(3)), rHH(x1H, x0H),HFH(x1H, x0H)),
    Rule(4, Value(0.240, Token(4)), rMH(x1M, x0H),HFH(x1H, x0H)),
    Rule(5, Value(0.239, Token(5)), rRH(x1M, x0H),HFH(x1H, x0H)),
    Rule(6, Value(0.222, Token(6)), rMH(x1M, x0H),HFH(x1H, x0H)),
    Rule(7, Value(0.099, Token(7)), rHH(x1H, x0H),MmethRet(x1M, x0V)),
    Rule(8, Value(0.089, Token(8)), rRH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(9, Value(0.086, Token(9)), rHH(x1H, x0H),rMH(x1M, x0H)),
    Rule(10, Value(0.085, Token(10)), rHH(x1H, x0H),rHH(x1H, x0H)),
    Rule(11, Value(0.082, Token(11)), rMH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(12, Value(0.078, Token(12)), rMH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(13, Value(0.075, Token(13)), rMH(x1M, x0H),rMH(x1M, x0H)),
    Rule(14, Value(0.073, Token(14)), rRH(x1M, x0H),rHH(x1H, x0H)),
    Rule(15, Value(0.069, Token(15)), rHH(x2H, x0H),HFH(x1H, x0H), HFH(x2H, x1H)),
    Rule(16, Value(0.066, Token(16)), rHH(x1H, x0H),rRH(x1M, x0H)),
    Rule(17, Value(0.062, Token(17)), rRH(x2M, x0H),HFH(x1H, x0H), HFH(x2H, x1H)),
    Rule(18, Value(0.061, Token(18)), rHH(x1H, x0H),MmethArg(x1M, x0V)),
    Rule(19, Value(0.055, Token(19)), rMH(x1M, x0H),rHH(x1H, x0H)),
    Rule(20, Value(0.053, Token(20)), rRH(x1M, x0H),MmethArg(x1M, x0V)),
    Rule(21, Value(0.053, Token(21)), rMH(x2M, x0H),HFH(x1H, x0H), HFH(x2H, x1H)),
    Rule(22, Value(0.049, Token(22)), rRH(x1M, x0H),rRH(x1M, x0H)),
    Rule(23, Value(0.047, Token(23)), rMH(x1M, x0H),rRH(x1M, x0H)),
    Rule(24, Value(0.042, Token(24)), rHH(x1H, x0H),MmethRet(x1M, x0V)),
    Rule(25, Value(0.039, Token(25)), rRH(x1M, x0H),rMH(x1M, x0H)),
    Rule(26, Value(0.039, Token(26)), rMH(x1M, x0H),VH(x1V, x0H)),
    Rule(27, Value(0.037, Token(27)), rRH(x1M, x0H),MmethRet(x1M, x0V)),
    Rule(28, Value(0.034, Token(28)), rHH(x1H, x0H),VH(x1V, x0H)),
    Rule(29, Value(0.033, Token(29)), rRH(x1M, x0H),VH(x1V, x0H)),
    Rule(30, Value(0.033, Token(30)), rHH(x1H, x0H),rMH(x1M, x0H)),
    Rule(31, Value(0.033, Token(31)), rHH(x1H, x0H),rHH(x1H, x0H)),
  )

  val soup: Set[Rule] = Set(
    Rule(1,Value(0.5, Token(1)),rHH(x0H,x2H), HFH(x0H,x2H)),
    Rule(2,Value(0.5, Token(2)),rHH(x3H,x2H), HFH(x0H,x2H),rHH(x3H,x0H)),
    Rule(3,Value(0.5, Token(3)),rHH(x0H,x3H), HFH(x0H,x2H),rHH(x2H,x3H)),
    Rule(4,Value(0.5, Token(4)),rHH(x2H,x1H), rHH(x0H,x1H),rHH(x2H,x0H)),
    Rule(5,Value(0.5, Token(5)),rMH(x0M,x1H), rRH(x0M,x1H)),
    Rule(6,Value(0.5, Token(6)),rMH(x2M,x1H), MmethArg(x2M,x0V),VH(x0V,x1H)),
    Rule(7,Value(0.5, Token(7)),rMH(x2M,x1H), MmethRet(x2M,x0V),VH(x0V,x1H)),
    Rule(8,Value(0.5, Token(8)),rMH(x2M,x1H), rMH(x0M,x1H),rRH(x2M,x1H)),
    Rule(9,Value(0.5, Token(9)),rMH(x2M,x1H), rHH(x0H,x1H),rRH(x2M,x1H)),
    Rule(10,Value(0.5, Token(10)),rMH(x2M,x1H), VH(x0V,x1H),rRH(x2M,x1H)),
    Rule(11,Value(0.5, Token(11)),rMH(x3M,x2H), HFH(x0H,x2H),rRH(x3M,x0H)),
    Rule(12,Value(0.5, Token(12)),rMH(x3M,x2H), HFH(x0H,x2H),rMH(x3M,x0H)),
    Rule(13,Value(0.5, Token(13)),rMH(x0M,x1H), rHH(x1H,x2H),rRH(x0M,x1H)),
    Rule(14,Value(0.5, Token(14)),rMH(x0M,x2H), rHH(x1H,x2H),rRH(x0M,x1H)),
    Rule(15,Value(0.5, Token(15)),rMH(x0M,x2H), rHH(x1H,x2H),rMH(x0M,x1H)),
    Rule(16,Value(0.5, Token(16)),rRH(x0M,x1H), rMH(x0M,x1H)),
    Rule(17,Value(0.5, Token(17)),rRH(x2M,x1H), MmethArg(x2M,x0V),VH(x0V,x1H)),
    Rule(18,Value(0.5, Token(18)),rRH(x2M,x1H), MmethRet(x2M,x0V),VH(x0V,x1H)),
    Rule(19,Value(0.5, Token(19)),rRH(x2M,x1H), rMH(x2M,x1H),rRH(x0M,x1H)),
    Rule(20,Value(0.5, Token(20)),rRH(x2M,x1H), rHH(x0H,x1H),rMH(x2M,x1H)),
    Rule(21,Value(0.5, Token(21)),rRH(x2M,x1H), VH(x0V,x1H),rMH(x2M,x1H)),
    Rule(22,Value(0.5, Token(22)),rRH(x3M,x2H), HFH(x0H,x2H),rRH(x3M,x0H)),
    Rule(23,Value(0.5, Token(23)),rRH(x3M,x2H), HFH(x0H,x2H),rMH(x3M,x0H)),
    Rule(24,Value(0.5, Token(24)),rRH(x0M,x1H), rHH(x1H,x2H),rMH(x0M,x1H)),
    Rule(25,Value(0.5, Token(25)),rRH(x0M,x2H), rHH(x1H,x2H),rRH(x0M,x1H)),
    Rule(26,Value(0.5, Token(26)),rRH(x0M,x2H), rHH(x1H,x2H),rMH(x0M,x1H))
  )

  val soupProg: Program = Program("EscapeSoup", soup)
  val soupProg1: Program = Program("EscapeSoup", soup1)
  val soupProg2: Program = Program("EscapeSoup", soup2)
  val soupProg3: Program = Program("EscapeSoup", soup3)
  val soupProg4: Program = Program("EscapeSoup", soup4)
  val soupProg5: Program = Program("EscapeSoup", soup5)
  val soupProg6: Program = Program("EscapeSoup", soup6)
  val soupProg7: Program = Program("EscapeSoup", soup7)
  val soupProg8: Program = Program("EscapeSoup", soup8)
  val soupProg9: Program = Program("EscapeSoup", soup9)
  val soupProg10: Program = Program("EscapeSoup", soup10)

  test(s"NLP result ${soupProg.name}") {
    val scorer = new Scorer(edb, refOut)
    println("soup1")
    val evaluator1 = SeminaiveEvaluator(soupProg1)
    val out1 = evaluator1(edb)
    val rmse1 = scorer.rmse(out1)
    println(s"RMS Error : $rmse1.")

    println("soup2")
    val evaluator2 = SeminaiveEvaluator(soupProg2)
    val out2 = evaluator2(edb)
    val rmse2 = scorer.rmse(out2)
    println(s"RMS Error : $rmse2.")

    println("soup3")
    val evaluator3 = SeminaiveEvaluator(soupProg3)
    val out3 = evaluator3(edb)
    val rmse3 = scorer.rmse(out3)
    println(s"RMS Error : $rmse3.")

    println("soup4")
    val evaluator4 = SeminaiveEvaluator(soupProg4)
    val out4 = evaluator4(edb)
    val rmse4 = scorer.rmse(out4)
    println(s"RMS Error : $rmse4.")

    println("soup5")
    val evaluator5 = SeminaiveEvaluator(soupProg5)
    val out5 = evaluator5(edb)
    val rmse5 = scorer.rmse(out5)
    println(s"RMS Error : $rmse5.")

    println("soup6")
    val evaluator6 = SeminaiveEvaluator(soupProg6)
    val out6 = evaluator6(edb)
    val rmse6 = scorer.rmse(out6)
    println(s"RMS Error : $rmse6.")

    println("soup7")
    val evaluator7 = SeminaiveEvaluator(soupProg7)
    val out7 = evaluator7(edb)
    val rmse7 = scorer.rmse(out7)
    println(s"RMS Error : $rmse7.")

    println("soup8")
    val evaluator8 = SeminaiveEvaluator(soupProg8)
    val out8 = evaluator8(edb)
    val rmse8 = scorer.rmse(out8)
    println(s"RMS Error : $rmse8.")

    println("soup9")
    val evaluator9 = SeminaiveEvaluator(soupProg9)
    val out9 = evaluator9(edb)
    val rmse9 = scorer.rmse(out9)
    println(s"RMS Error : $rmse9.")

    println("soup10")
    val evaluator10 = SeminaiveEvaluator(soupProg10)
    val out10 = evaluator10(edb)
    val rmse10 = scorer.rmse(out10)
    println(s"RMS Error : $rmse10.")
  }
}
