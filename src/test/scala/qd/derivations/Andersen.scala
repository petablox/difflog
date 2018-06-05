package qd
package derivations

class Andersen extends Problem {
  override val name: String = "Andersen"

  val heapSet: Set[Atom] = Range(0, 8).map(i => Atom(i)).toSet
  val heap: Domain = Domain("Heap", heapSet)

  val pt: Relation = Relation("pt", heap, heap)
  val addr: Relation = Relation("addr", heap, heap)
  val assgn: Relation = Relation("assgn", heap, heap)
  val store: Relation = Relation("store", heap, heap)
  val load: Relation = Relation("load", heap, heap)

  val addrTuples: Set[DTuple] = Set((1, 2), (2, 3), (3, 5), (5, 6)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  val assgnTuples: Set[DTuple] = Set((4, 1)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  val storeTuples: Set[DTuple] = Set((4, 5)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  val loadTuples: Set[DTuple] = Set((7, 2)).map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  override val edb: Config[DValue] = Config(addr -> (Instance[DValue](addr) ++ addrTuples.map(t => t -> DValue.One).toMap),
    assgn -> (Instance[DValue](assgn) ++ assgnTuples.map(t => t -> DValue.One).toMap),
    store -> (Instance[DValue](store) ++ storeTuples.map(t => t -> DValue.One).toMap),
    load -> (Instance[DValue](load) ++ loadTuples.map(t => t -> DValue.One).toMap))

  val ptTuples: Set[DTuple] = Set((1, 2), (2, 3), (3, 5), (5, 6), (4, 2), (7, 5), (2, 6))
    .map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  
  val x0: Variable = Variable("x0", heap)
  val x1: Variable = Variable("x1", heap)
  val x2: Variable = Variable("x2", heap)
  val x3: Variable = Variable("x3", heap)

  override val soup: Set[Rule[DValue]] = Set(
    Rule(1, DValue(Set()), pt(x0, x1), addr(x0, x1)),
    Rule(2, DValue(Set()), pt(x0, x1), assgn(x0, x1)),
    Rule(3, DValue(Set()), pt(x0, x1), load(x0, x1)),
    Rule(4, DValue(Set()), pt(x0, x1), store(x0, x1)),
    Rule(5, DValue(Set()), pt(x2, x1), pt(x0, x1), pt(x2, x0)),
    Rule(6, DValue(Set()), pt(x2, x1), addr(x2, x0), pt(x0, x1)),
    Rule(7, DValue(Set()), pt(x2, x1), assgn(x2, x0), pt(x0, x1)),
    Rule(8, DValue(Set()), pt(x2, x1), load(x2, x0), pt(x0, x1)),
    Rule(9, DValue(Set()), pt(x2, x1), pt(x0, x1), store(x2, x0)),
    Rule(10, DValue(Set()), pt(x2, x1), addr(x0, x1), pt(x2, x0)),
    Rule(11, DValue(Set()), pt(x2, x1), assgn(x0, x1), pt(x2, x0)),
    Rule(12, DValue(Set()), pt(x2, x1), load(x0, x1), pt(x2, x0)),
    Rule(13, DValue(Set()), pt(x2, x1), pt(x2, x0), store(x0, x1)),
    Rule(14, DValue(Set()), pt(x3, x1), pt(x0, x1), pt(x2, x0), pt(x3, x2)),
    Rule(15, DValue(Set()), pt(x3, x1), addr(x3, x2), pt(x0, x1), pt(x2, x0)),
    Rule(16, DValue(Set()), pt(x3, x1), assgn(x3, x2), pt(x0, x1), pt(x2, x0)),
    Rule(17, DValue(Set()), pt(x3, x1), load(x3, x2), pt(x0, x1), pt(x2, x0)),
    Rule(18, DValue(Set()), pt(x3, x1), pt(x0, x1), pt(x2, x0), store(x3, x2)),
    Rule(19, DValue(Set()), pt(x3, x1), addr(x2, x0), pt(x0, x1), pt(x3, x2)),
    Rule(20, DValue(Set()), pt(x3, x1), assgn(x2, x0), pt(x0, x1), pt(x3, x2)),
    Rule(21, DValue(Set()), pt(x3, x1), load(x2, x0), pt(x0, x1), pt(x3, x2)),
    Rule(22, DValue(Set()), pt(x3, x1), pt(x0, x1), pt(x3, x2), store(x2, x0)),
    Rule(23, DValue(Set()), pt(x3, x1), store(x2, x0), pt(x0, x1), pt(x2, x3)),
    Rule(24, DValue(Set()), pt(x3, x1), addr(x0, x1), pt(x2, x0), pt(x3, x2)),
    Rule(25, DValue(Set()), pt(x3, x1), assgn(x0, x1), pt(x2, x0), pt(x3, x2)),
    Rule(26, DValue(Set()), pt(x3, x1), load(x0, x1), pt(x2, x0), pt(x3, x2)),
    Rule(27, DValue(Set()), pt(x3, x1), pt(x2, x0), pt(x3, x2), store(x0, x1))
  )
  override val maxVarCount: Int = 20
}
