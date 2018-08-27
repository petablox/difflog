package qd
package learner

import scala.util.Random
import org.scalatest.Ignore

class AndersenSoup50 extends Problem {
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
  override val edb: Config[FValue] = Config(addr -> (Instance[FValue](addr) ++ addrTuples.map(t => t -> FValue.One).toMap),
                                    assgn -> (Instance[FValue](assgn) ++ assgnTuples.map(t => t -> FValue.One).toMap),
                                    store -> (Instance[FValue](store) ++ storeTuples.map(t => t -> FValue.One).toMap),
                                    load -> (Instance[FValue](load) ++ loadTuples.map(t => t -> FValue.One).toMap))

  val ptTuples: Set[DTuple] = Set((1, 2), (2, 3), (3, 5), (5, 6), (4, 2), (7, 5), (2, 6))
                              .map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  override val refOut: Config[FValue] = Config(pt -> (Instance[FValue](pt) ++ ptTuples.map(t => t -> FValue.One).toMap))

  val x0: Variable = Variable("x0", heap)
  val x1: Variable = Variable("x1", heap)
  val x2: Variable = Variable("x2", heap)
  val x3: Variable = Variable("x3", heap)

  override val soup: Set[Rule[FValue]] = Set(
    Rule(1, FValue(0.5, Token(1)), pt(x0, x1), addr(x0, x1)),
    Rule(2, FValue(0.5, Token(2)), pt(x0, x1), assgn(x0, x1)),
    Rule(5, FValue(0.5, Token(5)), pt(x2, x1), pt(x0, x1), pt(x2, x0)),
    Rule(7, FValue(0.5, Token(7)), pt(x2, x1), assgn(x2, x0), pt(x0, x1)),
    Rule(8, FValue(0.5, Token(8)), pt(x2, x1), load(x2, x0), pt(x0, x1)),
    Rule(16, FValue(0.5, Token(16)), pt(x3, x1), assgn(x3, x2), pt(x0, x1), pt(x2, x0)),
    Rule(17, FValue(0.5, Token(17)), pt(x3, x1), load(x3, x2), pt(x0, x1), pt(x2, x0)),
    Rule(18, FValue(0.5, Token(18)), pt(x3, x1), pt(x0, x1), pt(x2, x0), store(x3, x2)),
    Rule(19, FValue(0.5, Token(19)), pt(x3, x1), addr(x2, x0), pt(x0, x1), pt(x3, x2)),
    Rule(20, FValue(0.5, Token(20)), pt(x3, x1), assgn(x2, x0), pt(x0, x1), pt(x3, x2)),
    Rule(22, FValue(0.5, Token(22)), pt(x3, x1), pt(x0, x1), pt(x3, x2), store(x2, x0)),
    Rule(23, FValue(0.5, Token(23)), pt(x3, x1), store(x2, x0), pt(x0, x1), pt(x2, x3)),
    Rule(25, FValue(0.5, Token(25)), pt(x3, x1), assgn(x0, x1), pt(x2, x0), pt(x3, x2)),
    Rule(27, FValue(0.5, Token(27)), pt(x3, x1), pt(x2, x0), pt(x3, x2), store(x0, x1))
  )

  override val expected: Set[Any] = Set(1, 7, 17, 23)
  override val maxVarCount: Int = 20
  
}
