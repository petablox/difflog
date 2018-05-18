package qd
package learner

import org.scalatest.{FunSuite, Ignore}

import scala.util.Random

class AndersenNLP extends FunSuite {

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

  // result/andersen_253.nlp
  val soup1: Set[Rule] = Set(
    Rule(1, Value(0.505, Token(1)), pt(x1, x0),addr(x1, x0)),
    Rule(2, Value(0.437, Token(2)), pt(x1, x0),addr(x1, x0)),
    Rule(3, Value(0.223, Token(3)), pt(x2, x0),addr(x1, x0), addr(x2, x1)),
    Rule(4, Value(0.112, Token(4)), pt(x1, x0),pt(x1, x0)),
    Rule(5, Value(0.072, Token(5)), pt(x1, x0),assgn(x1, x0)),
    Rule(6, Value(0.053, Token(6)), pt(x1, x0),store(x1, x0)),
    Rule(7, Value(0.049, Token(7)), pt(x2, x0),pt(x1, x0), addr(x2, x1)),
    Rule(8, Value(0.040, Token(8)), pt(x1, x0),load(x1, x0)),
  )
// result/andersen_34.nlp
  val soup2: Set[Rule] = Set(
    Rule(1, Value(0.409, Token(1)), pt(x1, x0),addr(x1, x0)),
    Rule(2, Value(0.396, Token(2)), pt(x1, x0),addr(x1, x0)),
    Rule(3, Value(0.163, Token(3)), pt(x2, x0),addr(x1, x0), addr(x2, x1)),
    Rule(4, Value(0.084, Token(4)), pt(x1, x0),assgn(x1, x0)),
    Rule(5, Value(0.070, Token(5)), pt(x1, x0),store(x1, x0)),
    Rule(6, Value(0.070, Token(6)), pt(x1, x0),load(x1, x0)),
    Rule(7, Value(0.063, Token(7)), pt(x1, x0),pt(x1, x0)),
    Rule(8, Value(0.033, Token(8)), pt(x2, x0),assgn(x1, x0), addr(x2, x1)),
  )
// result/andersen_3519.nlp
  val soup3: Set[Rule] = Set(
    Rule(1, Value(0.443, Token(1)), pt(x1, x0),addr(x1, x0)),
    Rule(2, Value(0.397, Token(2)), pt(x1, x0),addr(x1, x0)),
    Rule(3, Value(0.178, Token(3)), pt(x2, x0),addr(x1, x0), addr(x2, x1)),
    Rule(4, Value(0.083, Token(4)), pt(x1, x0),pt(x1, x0)),
    Rule(5, Value(0.074, Token(5)), pt(x1, x0),load(x1, x0)),
    Rule(6, Value(0.071, Token(6)), pt(x1, x0),assgn(x1, x0)),
    Rule(7, Value(0.046, Token(7)), pt(x1, x0),store(x1, x0)),
    Rule(8, Value(0.033, Token(8)), pt(x2, x0),pt(x1, x0), addr(x2, x1)),
  )
// result/andersen_42.nlp
  val soup4: Set[Rule] = Set(
    Rule(1, Value(0.254, Token(1)), pt(x1, x0),assgn(x1, x0)),
    Rule(2, Value(0.254, Token(2)), pt(x1, x0),addr(x1, x0)),
    Rule(3, Value(0.216, Token(3)), pt(x1, x0),assgn(x1, x0)),
    Rule(4, Value(0.148, Token(4)), pt(x1, x0),addr(x1, x0)),
    Rule(5, Value(0.101, Token(5)), pt(x1, x0),pt(x1, x0)),
    Rule(6, Value(0.056, Token(6)), pt(x2, x0),assgn(x1, x0), assgn(x2, x1)),
    Rule(7, Value(0.056, Token(7)), pt(x2, x0),addr(x1, x0), assgn(x2, x1)),
    Rule(8, Value(0.042, Token(8)), pt(x1, x0),store(x1, x0)),
    Rule(9, Value(0.038, Token(9)), pt(x2, x0),assgn(x1, x0), addr(x2, x1)),
    Rule(10, Value(0.038, Token(10)), pt(x2, x0),addr(x1, x0), addr(x2, x1)),
    Rule(11, Value(0.037, Token(11)), pt(x1, x0),load(x1, x0)),
  )
// result/andersen_481.nlp
  val soup5: Set[Rule] = Set(
    Rule(1, Value(0.305, Token(1)), pt(x1, x0),assgn(x1, x0)),
    Rule(2, Value(0.280, Token(2)), pt(x1, x0),assgn(x1, x0)),
    Rule(3, Value(0.177, Token(3)), pt(x1, x0),addr(x1, x0)),
    Rule(4, Value(0.097, Token(4)), pt(x1, x0),pt(x1, x0)),
    Rule(5, Value(0.085, Token(5)), pt(x2, x0),assgn(x1, x0), assgn(x2, x1)),
    Rule(6, Value(0.084, Token(6)), pt(x1, x0),addr(x1, x0)),
    Rule(7, Value(0.050, Token(7)), pt(x2, x0),addr(x1, x0), assgn(x2, x1)),
    Rule(8, Value(0.046, Token(8)), pt(x1, x0),store(x1, x0)),
    Rule(9, Value(0.036, Token(9)), pt(x1, x0),load(x1, x0)),
  )
// result/andersen_499.nlp
  val soup6: Set[Rule] = Set(
    Rule(1, Value(0.315, Token(1)), pt(x1, x0),assgn(x1, x0)),
    Rule(2, Value(0.314, Token(2)), pt(x1, x0),addr(x1, x0)),
    Rule(3, Value(0.285, Token(3)), pt(x1, x0),assgn(x1, x0)),
    Rule(4, Value(0.168, Token(4)), pt(x1, x0),addr(x1, x0)),
    Rule(5, Value(0.091, Token(5)), pt(x2, x0),assgn(x1, x0), assgn(x2, x1)),
    Rule(6, Value(0.090, Token(6)), pt(x2, x0),addr(x1, x0), assgn(x2, x1)),
    Rule(7, Value(0.072, Token(7)), pt(x1, x0),pt(x1, x0)),
    Rule(8, Value(0.053, Token(8)), pt(x2, x0),assgn(x1, x0), addr(x2, x1)),
    Rule(9, Value(0.053, Token(9)), pt(x2, x0),addr(x1, x0), addr(x2, x1)),
    Rule(10, Value(0.051, Token(10)), pt(x1, x0),store(x1, x0)),
    Rule(11, Value(0.046, Token(11)), pt(x1, x0),load(x1, x0)),
  )
// result/andersen_591.nlp
  val soup7: Set[Rule] = Set(
    Rule(1, Value(0.372, Token(1)), pt(x1, x0),assgn(x1, x0)),
    Rule(2, Value(0.342, Token(2)), pt(x1, x0),assgn(x1, x0)),
    Rule(3, Value(0.246, Token(3)), pt(x1, x0),addr(x1, x0)),
    Rule(4, Value(0.130, Token(4)), pt(x2, x0),assgn(x1, x0), assgn(x2, x1)),
    Rule(5, Value(0.100, Token(5)), pt(x1, x0),addr(x1, x0)),
    Rule(6, Value(0.093, Token(6)), pt(x1, x0),pt(x1, x0)),
    Rule(7, Value(0.086, Token(7)), pt(x2, x0),addr(x1, x0), assgn(x2, x1)),
    Rule(8, Value(0.047, Token(8)), pt(x1, x0),store(x1, x0)),
    Rule(9, Value(0.041, Token(9)), pt(x1, x0),load(x1, x0)),
    Rule(10, Value(0.038, Token(10)), pt(x2, x0),assgn(x1, x0), addr(x2, x1)),
  )
// result/andersen_6012.nlp
  val soup8: Set[Rule] = Set(
    Rule(1, Value(0.272, Token(1)), pt(x1, x0),assgn(x1, x0)),
    Rule(2, Value(0.241, Token(2)), pt(x1, x0),assgn(x1, x0)),
    Rule(3, Value(0.239, Token(3)), pt(x1, x0),addr(x1, x0)),
    Rule(4, Value(0.137, Token(4)), pt(x1, x0),pt(x1, x0)),
    Rule(5, Value(0.134, Token(5)), pt(x1, x0),addr(x1, x0)),
    Rule(6, Value(0.066, Token(6)), pt(x2, x0),assgn(x1, x0), assgn(x2, x1)),
    Rule(7, Value(0.058, Token(7)), pt(x2, x0),addr(x1, x0), assgn(x2, x1)),
    Rule(8, Value(0.055, Token(8)), pt(x1, x0),store(x1, x0)),
    Rule(9, Value(0.049, Token(9)), pt(x1, x0),pt(x1, x0)),
    Rule(10, Value(0.037, Token(10)), pt(x2, x0),assgn(x1, x0), addr(x2, x1)),
    Rule(11, Value(0.033, Token(11)), pt(x2, x0),pt(x1, x0), assgn(x2, x1)),
    Rule(12, Value(0.032, Token(12)), pt(x2, x0),addr(x1, x0), addr(x2, x1)),
  )
// result/andersen_68.nlp
  val soup9: Set[Rule] = Set(
    Rule(1, Value(0.352, Token(1)), pt(x1, x0),assgn(x1, x0)),
    Rule(2, Value(0.337, Token(2)), pt(x1, x0),assgn(x1, x0)),
    Rule(3, Value(0.283, Token(3)), pt(x1, x0),addr(x1, x0)),
    Rule(4, Value(0.133, Token(4)), pt(x1, x0),addr(x1, x0)),
    Rule(5, Value(0.118, Token(5)), pt(x2, x0),assgn(x1, x0), assgn(x2, x1)),
    Rule(6, Value(0.095, Token(6)), pt(x2, x0),addr(x1, x0), assgn(x2, x1)),
    Rule(7, Value(0.065, Token(7)), pt(x1, x0),store(x1, x0)),
    Rule(8, Value(0.047, Token(8)), pt(x2, x0),assgn(x1, x0), addr(x2, x1)),
    Rule(9, Value(0.047, Token(9)), pt(x1, x0),pt(x1, x0)),
    Rule(10, Value(0.038, Token(10)), pt(x2, x0),addr(x1, x0), addr(x2, x1)),
    Rule(11, Value(0.035, Token(11)), pt(x1, x0),load(x1, x0)),
  )
// result/andersen_991.nlp
  val soup10: Set[Rule] = Set(
    Rule(1, Value(0.260, Token(1)), pt(x1, x0),assgn(x1, x0)),
    Rule(2, Value(0.227, Token(2)), pt(x1, x0),assgn(x1, x0)),
    Rule(3, Value(0.224, Token(3)), pt(x1, x0),addr(x1, x0)),
    Rule(4, Value(0.136, Token(4)), pt(x1, x0),addr(x1, x0)),
    Rule(5, Value(0.101, Token(5)), pt(x1, x0),pt(x1, x0)),
    Rule(6, Value(0.059, Token(6)), pt(x2, x0),assgn(x1, x0), assgn(x2, x1)),
    Rule(7, Value(0.055, Token(7)), pt(x1, x0),store(x1, x0)),
    Rule(8, Value(0.051, Token(8)), pt(x2, x0),addr(x1, x0), assgn(x2, x1)),
    Rule(9, Value(0.040, Token(9)), pt(x1, x0),load(x1, x0)),
    Rule(10, Value(0.036, Token(10)), pt(x2, x0),assgn(x1, x0), addr(x2, x1)),
    Rule(11, Value(0.035, Token(11)), pt(x1, x0),pt(x1, x0)),
  )

  val soupProg: Program = Program("AndersonSoup", soup1)
  val soupProg1: Program = Program("AndersonSoup", soup1)
  val soupProg2: Program = Program("AndersonSoup", soup2)
  val soupProg3: Program = Program("AndersonSoup", soup3)
  val soupProg4: Program = Program("AndersonSoup", soup4)
  val soupProg5: Program = Program("AndersonSoup", soup5)
  val soupProg6: Program = Program("AndersonSoup", soup6)
  val soupProg7: Program = Program("AndersonSoup", soup7)
  val soupProg8: Program = Program("AndersonSoup", soup8)
  val soupProg9: Program = Program("AndersonSoup", soup9)
  val soupProg10: Program = Program("AndersonSoup", soup10)

  test(s"NLP result ${soupProg.name}") {
    val scorer = new Scorer(edb, refOut)
    println("soup1")
    val evaluator1 = SeminaiveEvaluator(soupProg1)
    val out1 = evaluator1(edb)
    val rmse1 = scorer.rmse(out1)
    println(s"RMS Error : $rmse1.")

    println("soup2")
    val evaluator2 = SeminaiveEvaluator(soupProg2)
    val out2 = evaluator2(edb)
    val rmse2 = scorer.rmse(out2)
    println(s"RMS Error : $rmse2.")

    println("soup3")
    val evaluator3 = SeminaiveEvaluator(soupProg3)
    val out3 = evaluator3(edb)
    val rmse3 = scorer.rmse(out3)
    println(s"RMS Error : $rmse3.")

    println("soup4")
    val evaluator4 = SeminaiveEvaluator(soupProg4)
    val out4 = evaluator4(edb)
    val rmse4 = scorer.rmse(out4)
    println(s"RMS Error : $rmse4.")

    println("soup5")
    val evaluator5 = SeminaiveEvaluator(soupProg5)
    val out5 = evaluator5(edb)
    val rmse5 = scorer.rmse(out5)
    println(s"RMS Error : $rmse5.")

    println("soup6")
    val evaluator6 = SeminaiveEvaluator(soupProg6)
    val out6 = evaluator6(edb)
    val rmse6 = scorer.rmse(out6)
    println(s"RMS Error : $rmse6.")

    println("soup7")
    val evaluator7 = SeminaiveEvaluator(soupProg7)
    val out7 = evaluator7(edb)
    val rmse7 = scorer.rmse(out7)
    println(s"RMS Error : $rmse7.")

    println("soup8")
    val evaluator8 = SeminaiveEvaluator(soupProg8)
    val out8 = evaluator8(edb)
    val rmse8 = scorer.rmse(out8)
    println(s"RMS Error : $rmse8.")

    println("soup9")
    val evaluator9 = SeminaiveEvaluator(soupProg9)
    val out9 = evaluator9(edb)
    val rmse9 = scorer.rmse(out9)
    println(s"RMS Error : $rmse9.")

    println("soup10")
    val evaluator10 = SeminaiveEvaluator(soupProg10)
    val out10 = evaluator10(edb)
    val rmse10 = scorer.rmse(out10)
    println(s"RMS Error : $rmse10.")
  }
}
