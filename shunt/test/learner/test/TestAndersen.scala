package qd
package learner

import scala.util.Random

class TestAndersen extends Problem {
  override val name: String = "Andersen"

  val heapSet: Set[Atom] = Range(0, 12).map(i => Atom(i)).toSet
  val heap: Domain = Domain("Heap", heapSet)

  val pt: Relation = Relation("pt", heap, heap)
  val addr: Relation = Relation("addr", heap, heap)
  val assgn: Relation = Relation("assgn", heap, heap)
  val store: Relation = Relation("store", heap, heap)
  val load: Relation = Relation("load", heap, heap)

  val addrTuples: Set[DTuple] = Set((2, 6), (4, 1), (3, 10), (0, 10), (1, 7), (10, 3)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  val assgnTuples: Set[DTuple] = Set((2,4), (6, 9), (6, 0), (7, 2), (3, 5)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  val storeTuples: Set[DTuple] = Set((10, 1)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  val loadTuples: Set[DTuple] = Set((6, 4), (1, 0), (4, 4), (0, 5), (8, 7)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  override val edb: Config[FValue] = Config(addr -> (Instance[FValue](addr) ++ addrTuples.map(t => t -> FValue.One).toMap),
                                    assgn -> (Instance[FValue](assgn) ++ assgnTuples.map(t => t -> FValue.One).toMap),
                                    store -> (Instance[FValue](store) ++ storeTuples.map(t => t -> FValue.One).toMap),
                                    load -> (Instance[FValue](load) ++ loadTuples.map(t => t -> FValue.One).toMap))

  val ptTuples: Set[DTuple] = Set((7, 3), (4, 7), (1, 3), (6, 6), (7, 7), (2, 1), (0, 10), (3, 7), (10, 3),
    (6, 7), (3, 3), (8, 1), (3, 10), (6, 10), (8, 10), (6, 3), (7, 10), (8, 6), (4, 1), (4, 10),
    (2, 6), (7, 1), (2, 10), (2, 3), (8, 7), (2, 7), (8, 3), (4, 6), (6, 1), (4, 3), (1, 7), (7, 6)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  override val refOut: Config[FValue] = Config(pt -> (Instance[FValue](pt) ++ ptTuples.map(t => t -> FValue.One).toMap))

  val x0: Variable = Variable("x0", heap)
  val x1: Variable = Variable("x1", heap)
  val x2: Variable = Variable("x2", heap)
  val x3: Variable = Variable("x3", heap)

	val soup_pre: Set[Rule[FValue]] = Set(
		Rule(1, FValue(0.990000, Token(1)), pt(x0,x1),addr(x0,x1)),
		Rule(2, FValue(0.010000, Token(2)), pt(x0,x1),assgn(x0,x1)),
		Rule(3, FValue(0.010000, Token(3)), pt(x0,x1),load(x0,x1)),
		Rule(4, FValue(0.010000, Token(4)), pt(x0,x1),store(x0,x1)),
		Rule(5, FValue(0.010000, Token(5)), pt(x2,x1),pt(x0,x1),pt(x2,x0)),
		Rule(6, FValue(0.010000, Token(6)), pt(x2,x1),addr(x2,x0),pt(x0,x1)),
		Rule(7, FValue(0.990000, Token(7)), pt(x2,x1),assgn(x2,x0),pt(x0,x1)),
		Rule(8, FValue(0.010000, Token(8)), pt(x2,x1),load(x2,x0),pt(x0,x1)),
		Rule(9, FValue(0.010000, Token(9)), pt(x2,x1),pt(x0,x1),store(x2,x0)),
		Rule(10, FValue(0.010000, Token(10)), pt(x2,x1),addr(x0,x1),pt(x2,x0)),
		Rule(11, FValue(0.449526, Token(11)), pt(x2,x1),assgn(x0,x1),pt(x2,x0)),
		Rule(12, FValue(0.172710, Token(12)), pt(x2,x1),load(x0,x1),pt(x2,x0)),
		Rule(13, FValue(0.997743, Token(13)), pt(x2,x1),pt(x2,x0),store(x0,x1)),
		Rule(14, FValue(0.010000, Token(14)), pt(x3,x1),pt(x0,x1),pt(x2,x0),pt(x3,x2)),
		Rule(15, FValue(0.010000, Token(15)), pt(x3,x1),addr(x3,x2),pt(x0,x1),pt(x2,x0)),
		Rule(16, FValue(0.010000, Token(16)), pt(x3,x1),assgn(x3,x2),pt(x0,x1),pt(x2,x0)),
		Rule(17, FValue(0.990000, Token(17)), pt(x3,x1),load(x3,x2),pt(x0,x1),pt(x2,x0)),
		Rule(18, FValue(0.817190, Token(18)), pt(x3,x1),pt(x0,x1),pt(x2,x0),store(x3,x2)),
		Rule(19, FValue(0.010000, Token(19)), pt(x3,x1),addr(x2,x0),pt(x0,x1),pt(x3,x2)),
		Rule(20, FValue(0.937924, Token(20)), pt(x3,x1),assgn(x2,x0),pt(x0,x1),pt(x3,x2)),
		Rule(21, FValue(0.202037, Token(21)), pt(x3,x1),load(x2,x0),pt(x0,x1),pt(x3,x2)),
		Rule(22, FValue(0.242890, Token(22)), pt(x3,x1),pt(x0,x1),pt(x3,x2),store(x2,x0)),
		Rule(23, FValue(0.990000, Token(23)), pt(x3,x1),store(x2,x0),pt(x0,x1),pt(x2,x3)),
		Rule(24, FValue(0.010000, Token(24)), pt(x3,x1),addr(x0,x1),pt(x2,x0),pt(x3,x2)),
		Rule(25, FValue(0.383521, Token(25)), pt(x3,x1),assgn(x0,x1),pt(x2,x0),pt(x3,x2)),
		Rule(26, FValue(0.788306, Token(26)), pt(x3,x1),load(x0,x1),pt(x2,x0),pt(x3,x2)),
		Rule(27, FValue(0.212196, Token(27)), pt(x3,x1),pt(x2,x0),pt(x3,x2),store(x0,x1)),
		)

  override val expected: Set[Any] = Set(1, 7, 17, 23)
  override val maxVarCount: Int = 20
  val usefulTokens= Set(1, 7, 9, 10, 17, 21, 23)
  val soup =
    soup_pre.map(r => Rule(r.name, FValue(1.0, r.coeff.prov), r.head, r.body)).
    filter(r => usefulTokens.contains(r.name.asInstanceOf[Int]))
}
