package qd
package derivations

class AndersenLarge extends Problem {
  override val name: String = "AndersenLarge"

  val heapSet: Set[Atom] = Range(0, 20).map(i => Atom(i)).toSet
  val heap: Domain = Domain("Heap", heapSet)

  val pt: Relation = Relation("pt", heap, heap)
  val addr: Relation = Relation("addr", heap, heap)
  val assgn: Relation = Relation("assgn", heap, heap)
  val store: Relation = Relation("store", heap, heap)
  val load: Relation = Relation("load", heap, heap)

  val assgnTuples = Set((15, 13),(5, 10),(12, 18),(15, 8),(19, 13),(4, 2)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
  val loadTuples = Set((0, 1),(4, 9),(16, 4),(10, 7),(7, 8),(11, 9),(7, 7),(6, 3),(11, 4),(0, 11),(2, 3),(0, 10),(10, 11),(12, 16),(14, 15),(7, 16),(6, 17),(6, 15),(2, 12)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
  //val ptTuples = Set((7, 3),(14, 17),(1, 3),(10, 17),(17, 7),(18, 15),(14, 7),(11, 2),(1, 2),(1, 17),(0, 7),(1, 15),(10, 15),(17, 3),(7, 15),(16, 2),(3, 7),(0, 3),(3, 17),(10, 3),(6, 17),(7, 2),(17, 15),(16, 17),(6, 3),(6, 7),(3, 3),(0, 15),(10, 7),(16, 3),(17, 2),(12, 17),(16, 7),(16, 15),(15, 3),(15, 15),(15, 17),(3, 15),(1, 1),(12, 3),(2, 7),(3, 2),(6, 18),(19, 6),(12, 2),(5, 17),(12, 7),(2, 2),(4, 7),(7, 7),(14, 15),(14, 3),(5, 15),(2, 3),(15, 7),(8, 17),(4, 2),(14, 2),(2, 17),(5, 3),(0, 1),(11, 7),(12, 15),(11, 17),(0, 2),(15, 2),(0, 17),(6, 15),(4, 15),(5, 7),(4, 17),(11, 3),(1, 7),(6, 2),(4, 3),(7, 17),(2, 15),(11, 15),(5, 2),(17, 17),(10, 2),(0, 3),(0, 4),(1, 1),(1, 4),(1, 7),(1, 18),(1, 19),(2, 13),(2, 17),(3, 11),(3, 18),(4, 5),(4, 6),(4, 10),(4, 14),(5, 8),(5, 11),(5, 12),(5, 14),(6, 3),(7, 3),(7, 14),(7, 17),(9, 18),(10, 14),(11, 3),(11, 4),(11, 7),(11, 8),(11, 13),(15, 2),(15, 3),(16, 1),(17, 0),(17, 7),(18, 2),(18, 7),(18, 18),(19, 9),(19, 11)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
  val addrTuples = Set((7, 3),(6, 18),(19, 6),(10, 7),(18, 15),(8, 17),(2, 15),(0, 3),(14, 2),(1, 1)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
  val storeTuples = Set((3, 15),(7, 14),(5, 9),(4, 5),(0, 7),(19, 12),(7, 10),(2, 5)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
  override val edb: Config[DValue] = Config(addr -> (Instance[DValue](addr) ++ addrTuples.map(t => t -> DValue.One).toMap),
    assgn -> (Instance[DValue](assgn) ++ assgnTuples.map(t => t -> DValue.One).toMap),
    store -> (Instance[DValue](store) ++ storeTuples.map(t => t -> DValue.One).toMap),
    load -> (Instance[DValue](load) ++ loadTuples.map(t => t -> DValue.One).toMap))
  
  val x0: Variable = Variable("x0", heap)
  val x1: Variable = Variable("x1", heap)
  val x2: Variable = Variable("x2", heap)
  val x3: Variable = Variable("x3", heap)

  override val soup: Set[Rule[DValue]] = Set(
    Rule(1, DValue(Set(Set(1))), pt(x0, x1), addr(x0, x1)),
    Rule(2, DValue(Set(Set(2))), pt(x0, x1), assgn(x0, x1)),
    Rule(3, DValue(Set(Set(3))), pt(x0, x1), load(x0, x1)),
    Rule(4, DValue(Set(Set(4))), pt(x0, x1), store(x0, x1)),
    Rule(5, DValue(Set(Set(5))), pt(x2, x1), pt(x0, x1), pt(x2, x0)),
    Rule(6, DValue(Set(Set(6))), pt(x2, x1), addr(x2, x0), pt(x0, x1)),
    Rule(7, DValue(Set(Set(7))), pt(x2, x1), assgn(x2, x0), pt(x0, x1)),
    Rule(8, DValue(Set(Set(8))), pt(x2, x1), load(x2, x0), pt(x0, x1)),
    Rule(9, DValue(Set(Set(9))), pt(x2, x1), pt(x0, x1), store(x2, x0)),
    Rule(10, DValue(Set(Set(10))), pt(x2, x1), addr(x0, x1), pt(x2, x0)),
    Rule(11, DValue(Set(Set(11))), pt(x2, x1), assgn(x0, x1), pt(x2, x0)),
    Rule(12, DValue(Set(Set(12))), pt(x2, x1), load(x0, x1), pt(x2, x0)),
    Rule(13, DValue(Set(Set(13))), pt(x2, x1), pt(x2, x0), store(x0, x1)),
    Rule(14, DValue(Set(Set(14))), pt(x3, x1), pt(x0, x1), pt(x2, x0), pt(x3, x2)),
    Rule(15, DValue(Set(Set(15))), pt(x3, x1), addr(x3, x2), pt(x0, x1), pt(x2, x0)),
    Rule(16, DValue(Set(Set(16))), pt(x3, x1), assgn(x3, x2), pt(x0, x1), pt(x2, x0)),
    Rule(17, DValue(Set(Set(17))), pt(x3, x1), load(x3, x2), pt(x0, x1), pt(x2, x0)),
    Rule(18, DValue(Set(Set(18))), pt(x3, x1), pt(x0, x1), pt(x2, x0), store(x3, x2)),
    Rule(19, DValue(Set(Set(19))), pt(x3, x1), addr(x2, x0), pt(x0, x1), pt(x3, x2)),
    Rule(20, DValue(Set(Set(20))), pt(x3, x1), assgn(x2, x0), pt(x0, x1), pt(x3, x2)),
    Rule(21, DValue(Set(Set(21))), pt(x3, x1), load(x2, x0), pt(x0, x1), pt(x3, x2)),
    Rule(22, DValue(Set(Set(22))), pt(x3, x1), pt(x0, x1), pt(x3, x2), store(x2, x0)),
    Rule(23, DValue(Set(Set(23))), pt(x3, x1), store(x2, x0), pt(x0, x1), pt(x2, x3)),
    Rule(24, DValue(Set(Set(24))), pt(x3, x1), addr(x0, x1), pt(x2, x0), pt(x3, x2)),
    Rule(25, DValue(Set(Set(25))), pt(x3, x1), assgn(x0, x1), pt(x2, x0), pt(x3, x2)),
    Rule(26, DValue(Set(Set(26))), pt(x3, x1), load(x0, x1), pt(x2, x0), pt(x3, x2)),
    Rule(27, DValue(Set(Set(27))), pt(x3, x1), pt(x2, x0), pt(x3, x2), store(x0, x1))
  )
  override val maxVarCount: Int = 20
}
