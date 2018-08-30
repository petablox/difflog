package qd
package learner

import scala.util.Random

class TestAndersenNLPPaper extends Problem {
  override val name: String = "Andersen"

  val heapSet: Set[Atom] = Range(0, 8).map(i => Atom(i)).toSet
  val heap: Domain = Domain("Heap", heapSet)

  val pt: Relation = Relation("pt", heap, heap)
  val addr: Relation = Relation("addr", heap, heap)
  val assgn: Relation = Relation("assgn", heap, heap)
  val store: Relation = Relation("store", heap, heap)
  val load: Relation = Relation("load", heap, heap)

  val addrTuples: Set[DTuple] = Set((2, 4), (3, 5), (0, 2), (0, 3), (0, 6)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  val assgnTuples: Set[DTuple] = Set((1,0)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  val storeTuples: Set[DTuple] = Set((0,7)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  val loadTuples: Set[DTuple] = Set((7,0)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  override val edb: Config[FValue] = Config(addr -> (Instance[FValue](addr) ++ addrTuples.map(t => t -> FValue.One).toMap),
                                    assgn -> (Instance[FValue](assgn) ++ assgnTuples.map(t => t -> FValue.One).toMap),
                                    store -> (Instance[FValue](store) ++ storeTuples.map(t => t -> FValue.One).toMap),
                                    load -> (Instance[FValue](load) ++ loadTuples.map(t => t -> FValue.One).toMap))

  val ptTuples: Set[DTuple] = Set((0, 2), (0, 3), (0, 6), (1, 2), (1, 3), (1, 6), (2, 4), (3, 5), (6, 4), (6, 5), (7, 4), (7, 5)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  override val refOut: Config[FValue] = Config(pt -> (Instance[FValue](pt) ++ ptTuples.map(t => t -> FValue.One).toMap))

  val x0: Variable = Variable("x0", heap)
  val x1: Variable = Variable("x1", heap)
  val x2: Variable = Variable("x2", heap)
  val x3: Variable = Variable("x3", heap)

	val soup: Set[Rule[FValue]] = Set(
    Rule(1, FValue(0.982, Token(1)), pt(x1, x0),assgn(x1, x0)),
    Rule(2, FValue(0.194, Token(2)), pt(x2, x0),assgn(x2, x1),assgn(x1,x0)),
    Rule(3, FValue(0.106, Token(3)), pt(x2, x0),addr(x2, x0)),
    Rule(4, FValue(0.105, Token(4)), pt(x1, x0),store(x1, x0)),
    Rule(5, FValue(0.045, Token(5)), pt(x2, x0),addr(x1, x0), assgn(x2, x1)),
    Rule(6, FValue(0.044, Token(6)), pt(x2, x0),store(x1, x0),assgn(x2, x1)),
    Rule(7, FValue(0.044, Token(7)), pt(x2, x0),pt(x2, x0)),
    Rule(8, FValue(0.038, Token(8)), pt(x1, x0),load(x1, x0)),
		)

  override val expected: Set[Any] = Set()
  override val maxVarCount: Int = 20
}