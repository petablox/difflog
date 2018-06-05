package qd
package learner

class TestEscape extends Problem {
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

  override val edb : Config[FValue] = Config(
    MmethArg -> (Instance[FValue](MmethArg) ++ MmethArgTuples.map(t => t -> FValue.One).toMap),
    MmethRet -> (Instance[FValue](MmethRet) ++ MmethRetTuples.map(t => t -> FValue.One).toMap),
    VH -> (Instance[FValue](VH) ++ VHTuples.map(t => t -> FValue.One).toMap),
    HFH -> (Instance[FValue](HFH) ++ HFHTuples.map(t => t -> FValue.One).toMap)
  )

  override val refOut : Config[FValue] = Config (
    rMH -> (Instance[FValue](rMH) ++ rMHTuples.map(t => t -> FValue.One).toMap),
    rRH -> (Instance[FValue](rRH) ++ rRHTuples.map(t => t -> FValue.One).toMap),
    rHH -> (Instance[FValue](rHH) ++ rHHTuples.map(t => t -> FValue.One).toMap)
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
  val soup_pre: Set[Rule[FValue]] = Set(
		Rule(1, FValue(0.990000, Token(1)), rHH(x0H,x2H),HFH(x0H,x2H)),
		Rule(2, FValue(0.243132, Token(2)), rHH(x3H,x2H),HFH(x0H,x2H),rHH(x3H,x0H)),
		Rule(3, FValue(0.990000, Token(3)), rHH(x0H,x3H),HFH(x0H,x2H),rHH(x2H,x3H)),
		Rule(4, FValue(0.221704, Token(4)), rHH(x2H,x1H),rHH(x0H,x1H),rHH(x2H,x0H)),
		Rule(5, FValue(0.010000, Token(5)), rMH(x0M,x1H),rRH(x0M,x1H)),
		Rule(6, FValue(0.990000, Token(6)), rMH(x2M,x1H),MmethArg(x2M,x0V),VH(x0V,x1H)),
		Rule(7, FValue(0.010000, Token(7)), rMH(x2M,x1H),MmethRet(x2M,x0V),VH(x0V,x1H)),
		Rule(8, FValue(0.010000, Token(8)), rMH(x2M,x1H),rMH(x0M,x1H),rRH(x2M,x1H)),
		Rule(9, FValue(0.010000, Token(9)), rMH(x2M,x1H),rHH(x0H,x1H),rRH(x2M,x1H)),
		Rule(10, FValue(0.010000, Token(10)), rMH(x2M,x1H),VH(x0V,x1H),rRH(x2M,x1H)),
		Rule(11, FValue(0.990000, Token(11)), rMH(x3M,x2H),HFH(x0H,x2H),rRH(x3M,x0H)),
		Rule(12, FValue(0.990000, Token(12)), rMH(x3M,x2H),HFH(x0H,x2H),rMH(x3M,x0H)),
		Rule(13, FValue(0.010000, Token(13)), rMH(x0M,x1H),rHH(x1H,x2H),rRH(x0M,x1H)),
		Rule(14, FValue(0.990000, Token(14)), rMH(x0M,x2H),rHH(x1H,x2H),rRH(x0M,x1H)),
		Rule(15, FValue(0.094296, Token(15)), rMH(x0M,x2H),rHH(x1H,x2H),rMH(x0M,x1H)),
		Rule(16, FValue(0.010000, Token(16)), rRH(x0M,x1H),rMH(x0M,x1H)),
		Rule(17, FValue(0.010000, Token(17)), rRH(x2M,x1H),MmethArg(x2M,x0H),VH(x0V,x1H)),
		Rule(18, FValue(0.990000, Token(18)), rRH(x2M,x1H),MmethRet(x2M,x0V),VH(x0V,x1H)),
		Rule(19, FValue(0.010000, Token(19)), rRH(x2M,x1H),rMH(x2M,x1H),rRH(x0M,x1H)),
		Rule(20, FValue(0.010000, Token(20)), rRH(x2M,x1H),rHH(x0H,x1H),rMH(x2M,x1H)),
		Rule(21, FValue(0.010000, Token(21)), rRH(x2M,x1H),VH(x0V,x1H),rMH(x2M,x1H)),
		Rule(22, FValue(0.990000, Token(22)), rRH(x3M,x2H),HFH(x0H,x2H),rRH(x3M,x0H)),
		Rule(23, FValue(0.990000, Token(23)), rRH(x3M,x2H),HFH(x0H,x2H),rMH(x3M,x0H)),
		Rule(24, FValue(0.010000, Token(24)), rRH(x0M,x1H),rHH(x1H,x2H),rMH(x0M,x1H)),
		Rule(25, FValue(0.061999, Token(25)), rRH(x0M,x2H),rHH(x1H,x2H),rRH(x0M,x1H)),
		Rule(26, FValue(0.365846, Token(26)), rRH(x0M,x2H),rHH(x1H,x2H),rMH(x0M,x1H)),
  )

  override val expected: Set[Any] = Set(1, 4, 6, 14, 18, 22)
  override val maxVarCount: Int = 20
  val usefulTokens= Set(1, 3, 6, 12, 18, 25)
  val soup =
    soup_pre.map(r => Rule(r.name, FValue(1.0, r.coeff.prov), r.head, r.body)).
    filter(r => usefulTokens.contains(r.name.asInstanceOf[Int]))
}
