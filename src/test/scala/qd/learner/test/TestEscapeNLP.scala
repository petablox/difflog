package qd
package learner

class TestEscapeNLP extends Problem {
  override val name: String = "Escape"

  val methodSet : Set[Atom] = Range(0, 8).map(i => Atom(i)).toSet
  val method : Domain = Domain("Method", methodSet)

  val variableSet : Set[Atom] = Range(0, 8).map(i => Atom(i)).toSet
  val variable : Domain = Domain("Variable", variableSet)

  val heapSet : Set[Atom] = Range(0, 8).map(i => Atom(i)).toSet
  val heap : Domain = Domain("Heap", heapSet)

  val MmethArg : Relation = Relation("MmethArg", method, variable)
  val MmethRet : Relation = Relation("MmethRet", method, variable)
  val VH : Relation = Relation("VH", variable, heap)
  val HFH : Relation = Relation("HFH", heap, heap)
  val rMH : Relation = Relation("rMH", method, heap)
  val rRH : Relation = Relation("rRH", method, heap)
  val rHH : Relation = Relation("rHH", heap, heap)

  val MmethArgTuples : Set[DTuple] = Set((3, 7), (3, 4), (1, 1)).map{ case (a,b) => DTuple(Atom(a), Atom(b)) }
  val MmethRetTuples : Set[DTuple] = Set((0, 1), (6, 4), (4, 7), (0, 0),
    (7, 3), (6, 6), (3, 1), (5, 2), (1, 1)).map { case (a,b) => DTuple(Atom(a), Atom(b)) }
  val VHTuples : Set[DTuple] = Set((3, 2), (2, 6), (4, 5), (1, 4), (5, 6), (3, 7)).map{ case (a,b) => DTuple(Atom(a), Atom(b)) }
  val HFHTuples : Set[DTuple] = Set((1, 2), (4, 6), (5, 6), (6, 1), (3, 7), (3, 5)).map{ case (a,b) => DTuple(Atom(a), Atom(b)) }
  val rMHTuples : Set[DTuple] = Set((1, 2), (3, 2), (3, 1), (1, 4), (1, 6), (3, 6), (1, 1), (3, 5)).map{ case (a,b) => DTuple(Atom(a), Atom(b)) }
  val rRHTuples : Set[DTuple] = Set((0, 1), (1, 2), (3, 2), (6, 6), (5, 6), (5, 2), (6, 1), (3, 1), (7, 7), (1, 4),
    (0, 2), (0, 6), (6, 2), (1, 6), (7, 2), (3, 6), (0, 4), (5, 1), (3, 4), (1, 1), (6, 5)).map{ case (a,b) => DTuple(Atom(a), Atom(b)) }
  val rHHTuples : Set[DTuple] = Set((1, 2), (3, 2), (4, 6), (5, 6), (5, 2), (6, 1), (3, 1), (4, 2), (6, 2), (3, 6), (5, 1), (3, 7), (4, 1), (3, 5)).map{ case (a,b) => DTuple(Atom(a), Atom(b)) }

  override val edb : Config = Config(
    MmethArg -> (Instance(MmethArg) ++ MmethArgTuples.map(t => t -> One).toMap),
    MmethRet -> (Instance(MmethRet) ++ MmethRetTuples.map(t => t -> One).toMap),
    VH -> (Instance(VH) ++ VHTuples.map(t => t -> One).toMap),
    HFH -> (Instance(HFH) ++ HFHTuples.map(t => t -> One).toMap)
  )

  override val refOut : Config = Config (
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

  // Expected: 1, 4, 6, 14, 18, 22
  val soup: Set[Rule] = Set(
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

  override val expected: Set[Any] = Set(1, 4, 6, 14, 18, 22)
  override val maxVarCount: Int = 20
}
