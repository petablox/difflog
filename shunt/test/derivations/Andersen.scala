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

  val soup2: Set[Rule[DValue]] = Set(

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
    Rule(27, DValue(Set(Set(27))), pt(x3, x1), pt(x2, x0), pt(x3, x2), store(x0, x1)),
    Rule(1, DValue(Set(Set(1))), pt(x0, x1), addr(x0, x1)),
    Rule(2, DValue(Set(Set(2))), pt(x0, x1), assgn(x0, x1)),
    Rule(3, DValue(Set(Set(3))), pt(x0, x1), load(x0, x1)),
    Rule(4, DValue(Set(Set(4))), pt(x0, x1), store(x0, x1)),
    Rule(5, DValue(Set(Set(5))), pt(x2, x1), pt(x0, x1), pt(x2, x0)),
    Rule(6, DValue(Set(Set(6))), pt(x2, x1), addr(x2, x0), pt(x0, x1)),
    Rule(7, DValue(Set(Set(7))), pt(x2, x1), assgn(x2, x0), pt(x0, x1)),
    Rule(8, DValue(Set(Set(8))), pt(x2, x1), load(x2, x0), pt(x0, x1)),
    Rule(9, DValue(Set(Set(9))), pt(x2, x1), pt(x0, x1), store(x2, x0)),
  )
  override val maxVarCount: Int = 20


  def p1: Program[DValue] = Program(s"Soup-$name", soup2.filter(_.freeVariables.size <= maxVarCount))
  // Seems to be a specific detail about how the soup is generated, here
  ignore(s"Comparing multiple iterations of Andersen ") {
    val startTime = System.nanoTime()
    val evaluator = SeminaiveEvaluator(p0)
    val evaluator2 = SeminaiveEvaluator(p1)
    val out1 : Config[DValue] = evaluator(edb)
    val out2 = evaluator2(edb)
    var num_clauses = 1
    println("")
    println(s"Printing information for ${name} results")
    for (x <- out1.iterator ) {
      println(x._1)
      for (y <- x._2.support) {
        num_clauses += y._2.getUnderlying.r.size
        if (y._2 != out2(x._1)(y._1)) {
          println(s"Value for tuple ${(y._1)} in out1: ${(y._2)}")
          println(s"Value for tuple ${(y._1)} in out2: ${out2(x._1)(y._1)}")
        }
      }
    }
    println(s"Total number of clauses: ${num_clauses}")
    val endTime = System.nanoTime()
    println(s"Evaluation finished in ${(endTime - startTime) / 1.0e9} seconds.")
  }
}
