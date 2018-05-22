package qd
package learner

import scala.util.{Random, Try}
import org.scalatest.{FunSuite, Ignore}

class AndersenScalability extends FunSuite {
  val name: String = "Andersen"

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
  val edb: Config = Config(addr -> (Instance(addr) ++ addrTuples.map(t => t -> One).toMap),
                                    assgn -> (Instance(assgn) ++ assgnTuples.map(t => t -> One).toMap),
                                    store -> (Instance(store) ++ storeTuples.map(t => t -> One).toMap),
                                    load -> (Instance(load) ++ loadTuples.map(t => t -> One).toMap))

  val ptTuples: Set[DTuple] = Set((1, 2), (2, 3), (3, 5), (5, 6), (4, 2), (7, 5), (2, 6))
                              .map { case (a, b) => DTuple(Atom(a), Atom(b)) }
  val refOut: Config = Config(pt -> (Instance(pt) ++ ptTuples.map(t => t -> One).toMap))

  val x0: Variable = Variable("x0", heap)
  val x1: Variable = Variable("x1", heap)
  val x2: Variable = Variable("x2", heap)
  val x3: Variable = Variable("x3", heap)

  val soup_pre: Seq[Rule] = Seq(
    Rule(1, Value(0.5, Token(1)), pt(x0, x1), addr(x0, x1)),
    Rule(7, Value(0.5, Token(7)), pt(x2, x1), assgn(x2, x0), pt(x0, x1)),
    Rule(17, Value(0.5, Token(17)), pt(x3, x1), load(x3, x2), pt(x0, x1), pt(x2, x0)),
    Rule(23, Value(0.5, Token(23)), pt(x3, x1), store(x2, x0), pt(x0, x1), pt(x2, x3)),
    Rule(21, Value(0.5, Token(21)), pt(x3, x1), load(x2, x0), pt(x0, x1), pt(x3, x2)),
    Rule(18, Value(0.5, Token(18)), pt(x3, x1), pt(x0, x1), pt(x2, x0), store(x3, x2)),
    Rule(24, Value(0.5, Token(24)), pt(x3, x1), addr(x0, x1), pt(x2, x0), pt(x3, x2)),
    Rule(3, Value(0.5, Token(3)), pt(x0, x1), load(x0, x1)),
    Rule(22, Value(0.5, Token(22)), pt(x3, x1), pt(x0, x1), pt(x3, x2), store(x2, x0)),
    Rule(27, Value(0.5, Token(27)), pt(x3, x1), pt(x2, x0), pt(x3, x2), store(x0, x1)),
    Rule(12, Value(0.5, Token(12)), pt(x2, x1), load(x0, x1), pt(x2, x0)),
    Rule(8, Value(0.5, Token(8)), pt(x2, x1), load(x2, x0), pt(x0, x1)),
    Rule(10, Value(0.5, Token(10)), pt(x2, x1), addr(x0, x1), pt(x2, x0)),
    Rule(26, Value(0.5, Token(26)), pt(x3, x1), load(x0, x1), pt(x2, x0), pt(x3, x2)),
    Rule(16, Value(0.5, Token(16)), pt(x3, x1), assgn(x3, x2), pt(x0, x1), pt(x2, x0)),
    Rule(2, Value(0.5, Token(2)), pt(x0, x1), assgn(x0, x1)),
    Rule(9, Value(0.5, Token(9)), pt(x2, x1), pt(x0, x1), store(x2, x0)),
    Rule(25, Value(0.5, Token(25)), pt(x3, x1), assgn(x0, x1), pt(x2, x0), pt(x3, x2)),
    Rule(20, Value(0.5, Token(20)), pt(x3, x1), assgn(x2, x0), pt(x0, x1), pt(x3, x2)),
    Rule(11, Value(0.5, Token(11)), pt(x2, x1), assgn(x0, x1), pt(x2, x0)),
    Rule(13, Value(0.5, Token(13)), pt(x2, x1), pt(x2, x0), store(x0, x1)),
    Rule(19, Value(0.5, Token(19)), pt(x3, x1), addr(x2, x0), pt(x0, x1), pt(x3, x2)),
    Rule(15, Value(0.5, Token(15)), pt(x3, x1), addr(x3, x2), pt(x0, x1), pt(x2, x0)),
    Rule(4, Value(0.5, Token(4)), pt(x0, x1), store(x0, x1)),
    Rule(14, Value(0.5, Token(14)), pt(x3, x1), pt(x0, x1), pt(x2, x0), pt(x3, x2)),
    Rule(5, Value(0.5, Token(5)), pt(x2, x1), pt(x0, x1), pt(x2, x0)),
    Rule(6, Value(0.5, Token(6)), pt(x2, x1), addr(x2, x0), pt(x0, x1)),
  )

  val expected: Set[Any] = Set(1, 7, 17, 23)
  val maxVarCount: Int = 20
  
  test(s"Scalability") {
    for (nrules <- Range(4, 28)) {
      val startTime = System.nanoTime()
      val seed = Try(sys.env.getOrElse("RANDOM_SEED", "0").toInt).getOrElse(0)
      val random: Random = new Random(seed)
      println(s"$nrules rules selected")
      val soup = soup_pre.take(nrules).toSet
      val p = Program(s"AndersenScalability", soup)
      val learner = new Learner(edb, refOut, p, random)
      learner.learn(0.01, 500)
      val endTime = System.nanoTime()
      println(s"Learning finished in ${(endTime - startTime) / 1.0e9} seconds.")
    }
  }
}
