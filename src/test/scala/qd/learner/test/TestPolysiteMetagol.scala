package qd
package learner

class TestPolysiteMetagol extends Problem {
  override val name: String = "Polysite"

  val ISet = Range(0, 8).map(i => Atom(i)).toSet
  val I = Domain("I", ISet)

  val CSet = Range(0, 2).map(i => Atom(i)).toSet
  val C = Domain("C", CSet)

  val MSet = Range(0, 12).map(i => Atom(i)).toSet
  val M = Domain("M", MSet)

  val CICM : Relation = Relation("CICM", C, I, M)
  val virtIM = Relation("virtIM", I, M)
  val Mneq = Relation("Mneq", M, M)

  val insvIM = Relation("insvIM", I, M)
  val virtI = Relation("virtI", I)
  val polySite = Relation("polySite", I)

  val CICMTuples = Set((1, 1, 0), (0, 4, 5), (1, 6, 1), (0, 2, 6), (1, 1, 2), (0, 0, 11), (1, 1, 1), (0, 3, 1), (0, 2, 4)).map{case (a,b,c) => DTuple(Atom(a), Atom(b), Atom(c))}
  val virtIMTuples = Set((1, 3), (6, 7), (6, 1), (0, 4), (0, 11), (7, 10), (1, 7), (0, 9), (3, 7), (1, 0), (5, 7)).map{case(a,b) => DTuple(Atom(a), Atom(b))}
  val pairs_list = for (i <- 0 to 11; j <- 0 to 11) yield (i,j)
  val filtered : IndexedSeq[(Int, Int)] = pairs_list.filter { case (a,b) => a != b }
  val MneqTuples : Set[DTuple] = Set(filtered : _*).map{ case (a) => DTuple(Atom(a._1), Atom(a._2)) }

  val insvIMTuples = Set((1, 2), (2, 6), (4, 5), (6, 1), (3, 1), (1, 1), (0, 11), (1, 0), (2, 4)).map{case (a,b) => DTuple(Atom(a), Atom(b))}
  val virtITuples = Set(0,1,3,5,6,7).map{ case (x) => DTuple(Atom(x)) }
  val polysiteTuples = Set(2, 1).map{ case (x) => DTuple(Atom(x)) }

  override val edb : Config = Config(
    CICM -> (Instance(CICM) ++ CICMTuples.map(t => t -> One).toMap),
    virtIM -> (Instance(virtIM) ++ virtIMTuples.map(t => t -> One).toMap),
    Mneq -> (Instance(Mneq) ++ MneqTuples.map(t => t -> One).toMap),
  )

  override val refOut : Config = Config (
    insvIM -> (Instance(insvIM) ++ insvIMTuples.map(t => t -> One).toMap),
    virtI -> (Instance(virtI) ++ virtITuples.map(t => t -> One).toMap),
    polySite -> (Instance(polySite) ++ polysiteTuples.map(t => t -> One).toMap)
  )

  val x0I : Variable = Variable("x0I", I)
  val x1I : Variable = Variable("x1I", I)
  val x2I : Variable = Variable("x2I", I)
  val x3I : Variable = Variable("x3I", I)
  val x4I : Variable = Variable("x4I", I)
  val x5I : Variable = Variable("x5I", I)

  val x0M : Variable = Variable("x0M", M)
  val x1M : Variable = Variable("x1M", M)
  val x2M : Variable = Variable("x2M", M)
  val x3M : Variable = Variable("x3M", M)
  val x4M : Variable = Variable("x4M", M)
  val x5M : Variable = Variable("x5M", M)

  val x0C : Variable = Variable("x0C", C)
  val x1C : Variable = Variable("x1C", C)
  val x2C : Variable = Variable("x2C", C)
  val x3C : Variable = Variable("x3C", C)
  val x4C : Variable = Variable("x4C", C)

  val soup : Set[Rule] = Set(
    Rule(1, Value(1.0, Token(1)), insvIM(x0I,x1M), CICM(x0C,x0I,x1M)),
    Rule(2, Value(1.0, Token(2)), polySite(x0I), virtIM(x0I,x2M)),
    Rule(3, Value(1.0, Token(3)), virtI(x0I), virtIM(x0I,x2M)) 
  )

  override val expected: Set[Any] = Set(2, 139, 160)
  override val maxVarCount: Int = 3
}
