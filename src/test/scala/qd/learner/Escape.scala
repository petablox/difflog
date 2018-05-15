package qd
package learner

import org.scalatest.Ignore

@Ignore
class Escape extends Problem {
  override val name: String = "Escape"

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
  override val soup: Set[Rule] = Set(
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

  override val expected: Set[Any] = Set(1, 4, 6, 14, 18, 22)
}