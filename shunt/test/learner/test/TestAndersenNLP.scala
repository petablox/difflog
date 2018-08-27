package qd
package learner

import scala.util.Random

class TestAndersenNLP extends Problem {
  override val name: String = "Andersen"

  val heapSet: Set[Atom] = Range(0, 16).map(i => Atom(i)).toSet
  val heap: Domain = Domain("Heap", heapSet)

  val pt: Relation = Relation("pt", heap, heap)
  val addr: Relation = Relation("addr", heap, heap)
  val assgn: Relation = Relation("assgn", heap, heap)
  val store: Relation = Relation("store", heap, heap)
  val load: Relation = Relation("load", heap, heap)

  val addrTuples: Set[DTuple] = Set( (0, 11), (0, 14), (0, 15), (1, 4), (1, 6), (1, 8), (1, 9), (1, 10), (1, 13), (2, 1),
    (2, 7), (2, 9), (2, 13), (3, 0), (3, 9), (3, 12), (4, 2), (4, 15), (5, 0), (5, 1), (5, 9), (5, 13), (5, 15), (6, 10),
    (6, 14), (6, 15), (7, 0), (7, 11), (7, 13), (7, 14), (8, 4), (8, 14), (10, 4), (10, 6), (11, 5), (11, 7), (11, 13),
    (12, 1), (12, 7), (12, 10), (12, 11), (13, 2), (13, 14), (14, 3), (14, 5),
    (14, 14), (15, 1), (15, 4), (15, 11)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  val assgnTuples: Set[DTuple] = Set((2, 7), (3, 8), (6, 1), (6, 12), (7, 14), (8, 11), (9, 15),
    (12, 10), (13, 13), (14, 9), (15, 5)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  val storeTuples: Set[DTuple] = Set((0, 11), (1, 10), (2, 15), (5, 7), (6, 4), (6, 6), (7, 9), (8, 10), (10, 9), (12, 9), (14, 2)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  val loadTuples: Set[DTuple] = Set((1, 7), (1, 12), (2, 3), (3, 4), (7, 4), (9, 3), (11, 11), (11, 12), (12, 8), (13, 12), (13, 15)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  override val edb: Config[FValue] = Config(addr -> (Instance[FValue](addr) ++ addrTuples.map(t => t -> FValue.One).toMap),
                                    assgn -> (Instance[FValue](assgn) ++ assgnTuples.map(t => t -> FValue.One).toMap),
                                    store -> (Instance[FValue](store) ++ storeTuples.map(t => t -> FValue.One).toMap),
                                    load -> (Instance[FValue](load) ++ loadTuples.map(t => t -> FValue.One).toMap))

  val ptTuples: Set[DTuple] = Set((7, 3), (6, 9), (12, 1), (11, 11), (7, 12), (14, 4), (13, 4), (12, 12), (0, 7),
    (15, 1), (1, 6), (0, 10), (3, 7), (2, 5), (1, 11), (8, 5), (5, 8), (4, 0), (10, 8), (9, 0), (6, 7), (5, 5), (11, 5),
    (10, 7), (7, 6), (6, 10), (12, 6), (15, 11), (14, 1), (13, 7), (0, 4), (15, 4), (1, 1), (8, 15), (4, 10), (3, 2),
    (2, 6), (9, 14), (8, 2), (5, 11), (4, 5), (10, 13), (9, 3), (6, 0), (11, 0), (7, 5), (14, 15), (12, 11), (15, 14),
    (14, 2), (13, 10), (0, 1), (3, 12), (1, 12), (8, 12), (4, 15), (3, 1), (2, 11), (9, 9), (5, 14), (10, 14), (6, 13),
    (11, 15), (7, 8), (14, 8), (13, 0), (12, 8), (15, 13), (13, 13), (0, 14), (3, 11), (2, 1), (1, 15), (8, 9), (4, 12),
    (2, 12), (9, 4), (5, 1), (10, 3), (7, 2), (6, 14), (12, 2), (11, 10), (7, 15), (14, 5), (13, 3), (12, 13), (15, 0),
    (1, 5), (0, 11), (3, 6), (2, 2), (1, 10), (8, 6), (4, 1), (10, 9), (9, 7), (6, 4), (5, 4), (11, 4), (10, 4), (7, 1),
    (6, 11), (12, 7), (11, 9), (15, 10), (14, 6), (13, 6), (0, 5), (15, 7), (1, 0), (0, 8), (4, 11), (3, 5), (2, 7), (9, 13),
    (8, 3), (5, 10), (4, 6), (10, 10), (9, 2), (6, 1), (5, 7), (11, 3), (7, 4), (14, 12), (12, 4), (15, 9), (14, 3), (13, 9),
    (0, 2), (3, 15), (1, 3), (8, 13), (4, 8), (3, 0), (2, 8), (9, 8), (8, 0), (5, 13), (10, 15), (6, 2), (11, 14), (7, 11),
    (14, 9), (12, 9), (15, 12), (13, 12), (0, 15), (3, 10), (1, 14), (8, 10), (4, 13), (2, 13), (9, 11), (5, 0), (10, 0),
    (6, 15), (12, 3), (11, 13), (7, 14), (14, 10), (13, 2), (12, 14), (15, 3), (13, 15), (1, 4), (0, 12), (3, 9), (2, 3),
    (1, 9), (8, 7), (4, 2), (2, 14), (9, 6), (6, 5), (5, 3), (11, 7), (10, 5), (7, 0), (6, 8), (12, 0), (11, 8), (7, 13),
    (14, 7), (13, 5), (0, 6), (15, 6), (1, 7), (0, 9), (3, 4), (2, 4), (9, 12), (8, 4), (5, 9), (4, 7), (10, 11), (9, 1),
    (6, 6), (5, 6), (11, 2), (10, 6), (7, 7), (14, 13), (12, 5), (15, 8), (14, 0), (13, 8), (0, 3), (15, 5), (3, 14),
    (1, 2), (8, 14), (4, 9), (3, 3), (2, 9), (9, 15), (8, 1), (5, 12), (4, 4), (10, 12), (6, 3), (11, 1), (7, 10),
    (14, 14), (12, 10), (15, 15), (13, 11), (0, 0), (3, 13), (1, 13), (8, 11), (4, 14), (2, 10), (9, 10), (5, 15),
    (10, 1), (6, 12), (11, 12), (7, 9), (14, 11), (13, 1), (12, 15), (15, 2), (13, 14), (0, 13), (3, 8), (2, 0), (1, 8),
    (8, 8), (4, 3), (2, 15), (9, 5), (5, 2), (11, 6), (10, 2)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  override val refOut: Config[FValue] = Config(pt -> (Instance[FValue](pt) ++ ptTuples.map(t => t -> FValue.One).toMap))

  val x0: Variable = Variable("x0", heap)
  val x1: Variable = Variable("x1", heap)
  val x2: Variable = Variable("x2", heap)
  val x3: Variable = Variable("x3", heap)

	val soup: Set[Rule[FValue]] = Set(
    Rule(1, FValue(0.505, Token(1)), pt(x1, x0),addr(x1, x0)),
    Rule(2, FValue(0.437, Token(2)), pt(x1, x0),addr(x1, x0)),
    Rule(3, FValue(0.223, Token(3)), pt(x2, x0),addr(x1, x0), addr(x2, x1)),
    Rule(4, FValue(0.112, Token(4)), pt(x1, x0),pt(x1, x0)),
    Rule(5, FValue(0.072, Token(5)), pt(x1, x0),assgn(x1, x0)),
    Rule(6, FValue(0.053, Token(6)), pt(x1, x0),store(x1, x0)),
    Rule(7, FValue(0.049, Token(7)), pt(x2, x0),pt(x1, x0), addr(x2, x1)),
    Rule(8, FValue(0.040, Token(8)), pt(x1, x0),load(x1, x0)),
		)

  override val expected: Set[Any] = Set(1, 7, 17, 23)
  override val maxVarCount: Int = 20
}
