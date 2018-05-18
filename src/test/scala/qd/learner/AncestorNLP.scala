package qd
package learner
import org.scalatest.{FunSuite, Ignore}

class AncestorNLP extends FunSuite {
  val name = "ancestor"
  val PSet = Range(0, 5).map(i => Atom(i)).toSet
  val P = Domain("P", PSet)
  val father = Relation("father", P,P)
  val mother = Relation("mother", P,P)
  val parent = Relation("parent", P,P)
  val ancestor = Relation("ancestor", P,P)
  val ancestorTuples = Set((1, 0),(4, 3),(2, 1),(3, 2),(2, 0),(3, 1),(3, 0),(4, 2),(4, 1),(4, 0)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
  val fatherTuples = Set((1, 0),(4, 3)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
  val parentTuples = Set((1, 0),(4, 3),(2, 1),(3, 2)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
  val motherTuples = Set((2, 1),(3, 2)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
  val edb = Config(
    father -> (Instance(father) ++ fatherTuples.map(t => t -> One).toMap),
    mother -> (Instance(mother) ++ motherTuples.map(t => t -> One).toMap),
    )
  val refOut = Config(
    parent -> (Instance(parent) ++ parentTuples.map(t => t -> One).toMap),
    ancestor -> (Instance(ancestor) ++ ancestorTuples.map(t => t -> One).toMap),
    )
  val x2P = Variable("x2P",P)
  val x0P = Variable("x0P",P)
  val x1P = Variable("x1P",P)
  val soup = Set(
    Rule(0, Value(0.5, Token(0)), parent(x0P,x1P),ancestor(x0P,x1P)),
    Rule(1, Value(0.5, Token(1)), parent(x0P,x1P),father(x0P,x1P)),
    Rule(2, Value(0.5, Token(2)), parent(x0P,x1P),mother(x0P,x1P)),
    Rule(3, Value(0.5, Token(3)), parent(x2P,x1P),ancestor(x0P,x1P),ancestor(x2P,x0P)),
    Rule(4, Value(0.5, Token(4)), parent(x2P,x1P),ancestor(x0P,x1P),parent(x2P,x0P)),
    Rule(5, Value(0.5, Token(5)), parent(x2P,x1P),ancestor(x0P,x1P),father(x2P,x0P)),
    Rule(6, Value(0.5, Token(6)), parent(x2P,x1P),ancestor(x0P,x1P),mother(x2P,x0P)),
    Rule(7, Value(0.5, Token(7)), parent(x2P,x1P),ancestor(x2P,x0P),parent(x0P,x1P)),
    Rule(8, Value(0.5, Token(8)), parent(x2P,x1P),parent(x0P,x1P),parent(x2P,x0P)),
    Rule(9, Value(0.5, Token(9)), parent(x2P,x1P),father(x2P,x0P),parent(x0P,x1P)),
    Rule(10, Value(0.5, Token(10)), parent(x2P,x1P),mother(x2P,x0P),parent(x0P,x1P)),
    Rule(11, Value(0.5, Token(11)), parent(x2P,x1P),ancestor(x2P,x0P),father(x0P,x1P)),
    Rule(12, Value(0.5, Token(12)), parent(x2P,x1P),father(x0P,x1P),parent(x2P,x0P)),
    Rule(13, Value(0.5, Token(13)), parent(x2P,x1P),father(x0P,x1P),father(x2P,x0P)),
    Rule(14, Value(0.5, Token(14)), parent(x2P,x1P),father(x0P,x1P),mother(x2P,x0P)),
    Rule(15, Value(0.5, Token(15)), parent(x2P,x1P),ancestor(x2P,x0P),mother(x0P,x1P)),
    Rule(16, Value(0.5, Token(16)), parent(x2P,x1P),mother(x0P,x1P),parent(x2P,x0P)),
    Rule(17, Value(0.5, Token(17)), parent(x2P,x1P),father(x2P,x0P),mother(x0P,x1P)),
    Rule(18, Value(0.5, Token(18)), parent(x2P,x1P),mother(x0P,x1P),mother(x2P,x0P)),
    Rule(19, Value(0.5, Token(19)), ancestor(x0P,x1P),parent(x0P,x1P)),
    Rule(20, Value(0.5, Token(20)), ancestor(x0P,x1P),father(x0P,x1P)),
    Rule(21, Value(0.5, Token(21)), ancestor(x0P,x1P),mother(x0P,x1P)),
    Rule(22, Value(0.5, Token(22)), ancestor(x2P,x1P),ancestor(x0P,x1P),ancestor(x2P,x0P)),
    Rule(23, Value(0.5, Token(23)), ancestor(x2P,x1P),ancestor(x0P,x1P),parent(x2P,x0P)),
    Rule(24, Value(0.5, Token(24)), ancestor(x2P,x1P),ancestor(x0P,x1P),father(x2P,x0P)),
    Rule(25, Value(0.5, Token(25)), ancestor(x2P,x1P),ancestor(x0P,x1P),mother(x2P,x0P)),
    Rule(26, Value(0.5, Token(26)), ancestor(x2P,x1P),ancestor(x2P,x0P),parent(x0P,x1P)),
    Rule(27, Value(0.5, Token(27)), ancestor(x2P,x1P),parent(x0P,x1P),parent(x2P,x0P)),
    Rule(28, Value(0.5, Token(28)), ancestor(x2P,x1P),father(x2P,x0P),parent(x0P,x1P)),
    Rule(29, Value(0.5, Token(29)), ancestor(x2P,x1P),mother(x2P,x0P),parent(x0P,x1P)),
    Rule(30, Value(0.5, Token(30)), ancestor(x2P,x1P),ancestor(x2P,x0P),father(x0P,x1P)),
    Rule(31, Value(0.5, Token(31)), ancestor(x2P,x1P),father(x0P,x1P),parent(x2P,x0P)),
    Rule(32, Value(0.5, Token(32)), ancestor(x2P,x1P),father(x0P,x1P),father(x2P,x0P)),
    Rule(33, Value(0.5, Token(33)), ancestor(x2P,x1P),father(x0P,x1P),mother(x2P,x0P)),
    Rule(34, Value(0.5, Token(34)), ancestor(x2P,x1P),ancestor(x2P,x0P),mother(x0P,x1P)),
    Rule(35, Value(0.5, Token(35)), ancestor(x2P,x1P),mother(x0P,x1P),parent(x2P,x0P)),
    Rule(36, Value(0.5, Token(36)), ancestor(x2P,x1P),father(x2P,x0P),mother(x0P,x1P)),
    Rule(37, Value(0.5, Token(37)), ancestor(x2P,x1P),mother(x0P,x1P),mother(x2P,x0P)),
    )

  // result/nlp/ancestor_253.nlp
  val soup1: Set[Rule] = Set(
    Rule(1, Value(0.303, Token(1)), ancestor(x1P, x0P),mother(x1P, x0P)),
    Rule(2, Value(0.251, Token(2)), ancestor(x1P, x0P),father(x1P, x0P)),
    Rule(3, Value(0.208, Token(3)), ancestor(x1P, x0P),mother(x1P, x0P)),
    Rule(4, Value(0.182, Token(4)), parent(x1P, x0P),mother(x1P, x0P)),
    Rule(5, Value(0.171, Token(5)), parent(x1P, x0P),father(x1P, x0P)),
    Rule(6, Value(0.173, Token(6)), ancestor(x1P, x0P),father(x1P, x0P)),
    Rule(7, Value(0.153, Token(7)), parent(x1P, x0P),parent(x1P, x0P)),
    Rule(8, Value(0.128, Token(8)), ancestor(x1P, x0P),ancestor(x1P, x0P)),
    Rule(9, Value(0.119, Token(9)), parent(x1P, x0P),mother(x1P, x0P)),
    Rule(10, Value(0.117, Token(10)), parent(x1P, x0P),ancestor(x1P, x0P)),
    Rule(11, Value(0.120, Token(11)), ancestor(x1P, x0P),parent(x1P, x0P)),
    Rule(12, Value(0.104, Token(12)), parent(x1P, x0P),father(x1P, x0P)),
    Rule(13, Value(0.084, Token(13)), parent(x1P, x0P),parent(x1P, x0P)),
    Rule(14, Value(0.064, Token(14)), ancestor(x2P, x0P),mother(x1P, x0P), mother(x2P, x1P)),
    Rule(15, Value(0.052, Token(15)), parent(x1P, x0P),ancestor(x1P, x0P)),
    Rule(16, Value(0.053, Token(16)), ancestor(x2P, x0P),mother(x1P, x0P), father(x2P, x1P)),
    Rule(17, Value(0.053, Token(17)), ancestor(x2P, x0P),father(x1P, x0P), mother(x2P, x1P)),
    Rule(18, Value(0.044, Token(18)), ancestor(x2P, x0P),father(x1P, x0P), father(x2P, x1P)),
    Rule(19, Value(0.043, Token(19)), ancestor(x1P, x0P),ancestor(x1P, x0P)),
    Rule(20, Value(0.041, Token(20)), ancestor(x1P, x0P),parent(x1P, x0P)),
  )
// result/nlp/ancestor_34.nlp
  val soup2: Set[Rule] = Set(
    Rule(1, Value(0.331, Token(1)), ancestor(x1P, x0P),mother(x1P, x0P)),
    Rule(2, Value(0.244, Token(2)), ancestor(x1P, x0P),father(x1P, x0P)),
    Rule(3, Value(0.231, Token(3)), ancestor(x1P, x0P),mother(x1P, x0P)),
    Rule(4, Value(0.178, Token(4)), parent(x1P, x0P),father(x1P, x0P)),
    Rule(5, Value(0.166, Token(5)), parent(x1P, x0P),mother(x1P, x0P)),
    Rule(6, Value(0.165, Token(6)), ancestor(x1P, x0P),father(x1P, x0P)),
    Rule(7, Value(0.137, Token(7)), parent(x1P, x0P),ancestor(x1P, x0P)),
    Rule(8, Value(0.118, Token(8)), parent(x1P, x0P),father(x1P, x0P)),
    Rule(9, Value(0.114, Token(9)), parent(x1P, x0P),parent(x1P, x0P)),
    Rule(10, Value(0.114, Token(10)), ancestor(x1P, x0P),ancestor(x1P, x0P)),
    Rule(11, Value(0.094, Token(11)), parent(x1P, x0P),mother(x1P, x0P)),
    Rule(12, Value(0.077, Token(12)), ancestor(x2P, x0P),mother(x1P, x0P), mother(x2P, x1P)),
    Rule(13, Value(0.077, Token(13)), ancestor(x1P, x0P),parent(x1P, x0P)),
    Rule(14, Value(0.070, Token(14)), parent(x1P, x0P),ancestor(x1P, x0P)),
    Rule(15, Value(0.057, Token(15)), ancestor(x2P, x0P),father(x1P, x0P), mother(x2P, x1P)),
    Rule(16, Value(0.055, Token(16)), ancestor(x2P, x0P),mother(x1P, x0P), father(x2P, x1P)),
    Rule(17, Value(0.049, Token(17)), parent(x1P, x0P),parent(x1P, x0P)),
    Rule(18, Value(0.041, Token(18)), ancestor(x2P, x0P),father(x1P, x0P), father(x2P, x1P)),
    Rule(19, Value(0.035, Token(19)), ancestor(x1P, x0P),ancestor(x1P, x0P)),
  )
// result/nlp/ancestor_3519.nlp
  val soup3: Set[Rule] = Set(
    Rule(1, Value(0.333, Token(1)), ancestor(x1P, x0P),mother(x1P, x0P)),
    Rule(2, Value(0.296, Token(2)), ancestor(x1P, x0P),father(x1P, x0P)),
    Rule(3, Value(0.223, Token(3)), parent(x1P, x0P),father(x1P, x0P)),
    Rule(4, Value(0.228, Token(4)), ancestor(x1P, x0P),mother(x1P, x0P)),
    Rule(5, Value(0.214, Token(5)), parent(x1P, x0P),mother(x1P, x0P)),
    Rule(6, Value(0.210, Token(6)), ancestor(x1P, x0P),father(x1P, x0P)),
    Rule(7, Value(0.160, Token(7)), parent(x1P, x0P),father(x1P, x0P)),
    Rule(8, Value(0.144, Token(8)), parent(x1P, x0P),mother(x1P, x0P)),
    Rule(9, Value(0.124, Token(9)), parent(x1P, x0P),parent(x1P, x0P)),
    Rule(10, Value(0.118, Token(10)), parent(x1P, x0P),ancestor(x1P, x0P)),
    Rule(11, Value(0.122, Token(11)), ancestor(x1P, x0P),parent(x1P, x0P)),
    Rule(12, Value(0.102, Token(12)), ancestor(x1P, x0P),ancestor(x1P, x0P)),
    Rule(13, Value(0.076, Token(13)), ancestor(x2P, x0P),mother(x1P, x0P), mother(x2P, x1P)),
    Rule(14, Value(0.070, Token(14)), ancestor(x2P, x0P),mother(x1P, x0P), father(x2P, x1P)),
    Rule(15, Value(0.068, Token(15)), ancestor(x2P, x0P),father(x1P, x0P), mother(x2P, x1P)),
    Rule(16, Value(0.063, Token(16)), ancestor(x2P, x0P),father(x1P, x0P), father(x2P, x1P)),
    Rule(17, Value(0.049, Token(17)), parent(x1P, x0P),parent(x1P, x0P)),
    Rule(18, Value(0.045, Token(18)), parent(x1P, x0P),ancestor(x1P, x0P)),
    Rule(19, Value(0.036, Token(19)), parent(x2P, x0P),father(x1P, x0P), father(x2P, x1P)),
    Rule(20, Value(0.035, Token(20)), parent(x2P, x0P),mother(x1P, x0P), father(x2P, x1P)),
    Rule(21, Value(0.033, Token(21)), ancestor(x1P, x0P),parent(x1P, x0P)),
    Rule(22, Value(0.032, Token(22)), parent(x2P, x0P),father(x1P, x0P), mother(x2P, x1P)),
  )
// result/nlp/ancestor_42.nlp
  val soup4: Set[Rule] = Set(
    Rule(1, Value(0.290, Token(1)), ancestor(x1P, x0P),mother(x1P, x0P)),
    Rule(2, Value(0.290, Token(2)), ancestor(x1P, x0P),father(x1P, x0P)),
    Rule(3, Value(0.215, Token(3)), ancestor(x1P, x0P),father(x1P, x0P)),
    Rule(4, Value(0.192, Token(4)), parent(x1P, x0P),mother(x1P, x0P)),
    Rule(5, Value(0.189, Token(5)), ancestor(x1P, x0P),mother(x1P, x0P)),
    Rule(6, Value(0.177, Token(6)), parent(x1P, x0P),father(x1P, x0P)),
    Rule(7, Value(0.137, Token(7)), parent(x1P, x0P),parent(x1P, x0P)),
    Rule(8, Value(0.136, Token(8)), ancestor(x1P, x0P),parent(x1P, x0P)),
    Rule(9, Value(0.130, Token(9)), parent(x1P, x0P),mother(x1P, x0P)),
    Rule(10, Value(0.127, Token(10)), parent(x1P, x0P),ancestor(x1P, x0P)),
    Rule(11, Value(0.113, Token(11)), parent(x1P, x0P),father(x1P, x0P)),
    Rule(12, Value(0.075, Token(12)), ancestor(x1P, x0P),ancestor(x1P, x0P)),
    Rule(13, Value(0.067, Token(13)), parent(x1P, x0P),parent(x1P, x0P)),
    Rule(14, Value(0.063, Token(14)), ancestor(x2P, x0P),mother(x1P, x0P), father(x2P, x1P)),
    Rule(15, Value(0.063, Token(15)), ancestor(x2P, x0P),father(x1P, x0P), father(x2P, x1P)),
    Rule(16, Value(0.056, Token(16)), parent(x1P, x0P),ancestor(x1P, x0P)),
    Rule(17, Value(0.055, Token(17)), ancestor(x2P, x0P),mother(x1P, x0P), mother(x2P, x1P)),
    Rule(18, Value(0.055, Token(18)), ancestor(x2P, x0P),father(x1P, x0P), mother(x2P, x1P)),
    Rule(19, Value(0.047, Token(19)), ancestor(x1P, x0P),parent(x1P, x0P)),
  )
// result/nlp/ancestor_481.nlp
  val soup5: Set[Rule] = Set(
    Rule(1, Value(0.387, Token(1)), ancestor(x1P, x0P),mother(x1P, x0P)),
    Rule(2, Value(0.344, Token(2)), ancestor(x1P, x0P),mother(x1P, x0P)),
    Rule(3, Value(0.200, Token(3)), parent(x1P, x0P),mother(x1P, x0P)),
    Rule(4, Value(0.168, Token(4)), parent(x1P, x0P),father(x1P, x0P)),
    Rule(5, Value(0.134, Token(5)), ancestor(x2P, x0P),mother(x1P, x0P), mother(x2P, x1P)),
    Rule(6, Value(0.129, Token(6)), parent(x1P, x0P),mother(x1P, x0P)),
    Rule(7, Value(0.126, Token(7)), parent(x1P, x0P),parent(x1P, x0P)),
    Rule(8, Value(0.123, Token(8)), parent(x1P, x0P),ancestor(x1P, x0P)),
    Rule(9, Value(0.103, Token(9)), parent(x1P, x0P),father(x1P, x0P)),
    Rule(10, Value(0.101, Token(10)), ancestor(x1P, x0P),ancestor(x1P, x0P)),
    Rule(11, Value(0.086, Token(11)), ancestor(x1P, x0P),parent(x1P, x0P)),
    Rule(12, Value(0.057, Token(12)), parent(x1P, x0P),parent(x1P, x0P)),
    Rule(13, Value(0.054, Token(13)), ancestor(x1P, x0P),father(x1P, x0P)),
    Rule(14, Value(0.052, Token(14)), parent(x1P, x0P),ancestor(x1P, x0P)),
    Rule(15, Value(0.035, Token(15)), ancestor(x2P, x0P),ancestor(x1P, x0P), mother(x2P, x1P)),
  )
// result/nlp/ancestor_499.nlp
  val soup6: Set[Rule] = Set(
    Rule(1, Value(0.239, Token(1)), ancestor(x1P, x0P),mother(x1P, x0P)),
    Rule(2, Value(0.237, Token(2)), ancestor(x1P, x0P),father(x1P, x0P)),
    Rule(3, Value(0.230, Token(3)), parent(x1P, x0P),father(x1P, x0P)),
    Rule(4, Value(0.188, Token(4)), ancestor(x1P, x0P),father(x1P, x0P)),
    Rule(5, Value(0.184, Token(5)), parent(x1P, x0P),father(x1P, x0P)),
    Rule(6, Value(0.182, Token(6)), parent(x1P, x0P),mother(x1P, x0P)),
    Rule(7, Value(0.152, Token(7)), ancestor(x1P, x0P),mother(x1P, x0P)),
    Rule(8, Value(0.135, Token(8)), ancestor(x1P, x0P),ancestor(x1P, x0P)),
    Rule(9, Value(0.123, Token(9)), parent(x1P, x0P),ancestor(x1P, x0P)),
    Rule(10, Value(0.110, Token(10)), parent(x1P, x0P),mother(x1P, x0P)),
    Rule(11, Value(0.104, Token(11)), ancestor(x1P, x0P),parent(x1P, x0P)),
    Rule(12, Value(0.091, Token(12)), parent(x1P, x0P),parent(x1P, x0P)),
    Rule(13, Value(0.054, Token(13)), ancestor(x1P, x0P),ancestor(x1P, x0P)),
    Rule(14, Value(0.049, Token(14)), parent(x1P, x0P),ancestor(x1P, x0P)),
    Rule(15, Value(0.045, Token(15)), ancestor(x2P, x0P),mother(x1P, x0P), father(x2P, x1P)),
    Rule(16, Value(0.044, Token(16)), ancestor(x2P, x0P),father(x1P, x0P), father(x2P, x1P)),
    Rule(17, Value(0.043, Token(17)), parent(x2P, x0P),father(x1P, x0P), father(x2P, x1P)),
    Rule(18, Value(0.036, Token(18)), ancestor(x2P, x0P),mother(x1P, x0P), mother(x2P, x1P)),
    Rule(19, Value(0.036, Token(19)), ancestor(x2P, x0P),father(x1P, x0P), mother(x2P, x1P)),
    Rule(20, Value(0.034, Token(20)), parent(x2P, x0P),mother(x1P, x0P), father(x2P, x1P)),
  )
// result/nlp/ancestor_591.nlp
  val soup7: Set[Rule] = Set(
    Rule(1, Value(0.265, Token(1)), ancestor(x1P, x0P),mother(x1P, x0P)),
    Rule(2, Value(0.245, Token(2)), ancestor(x1P, x0P),father(x1P, x0P)),
    Rule(3, Value(0.201, Token(3)), parent(x1P, x0P),mother(x1P, x0P)),
    Rule(4, Value(0.180, Token(4)), ancestor(x1P, x0P),father(x1P, x0P)),
    Rule(5, Value(0.176, Token(5)), ancestor(x1P, x0P),mother(x1P, x0P)),
    Rule(6, Value(0.148, Token(6)), parent(x1P, x0P),ancestor(x1P, x0P)),
    Rule(7, Value(0.145, Token(7)), parent(x1P, x0P),mother(x1P, x0P)),
    Rule(8, Value(0.122, Token(8)), parent(x1P, x0P),parent(x1P, x0P)),
    Rule(9, Value(0.115, Token(9)), ancestor(x1P, x0P),parent(x1P, x0P)),
    Rule(10, Value(0.110, Token(10)), ancestor(x1P, x0P),ancestor(x1P, x0P)),
    Rule(11, Value(0.100, Token(11)), parent(x1P, x0P),father(x1P, x0P)),
    Rule(12, Value(0.078, Token(12)), parent(x1P, x0P),ancestor(x1P, x0P)),
    Rule(13, Value(0.055, Token(13)), parent(x1P, x0P),parent(x1P, x0P)),
    Rule(14, Value(0.048, Token(14)), ancestor(x2P, x0P),mother(x1P, x0P), father(x2P, x1P)),
    Rule(15, Value(0.047, Token(15)), ancestor(x2P, x0P),mother(x1P, x0P), mother(x2P, x1P)),
    Rule(16, Value(0.044, Token(16)), ancestor(x2P, x0P),father(x1P, x0P), father(x2P, x1P)),
    Rule(17, Value(0.043, Token(17)), ancestor(x2P, x0P),father(x1P, x0P), mother(x2P, x1P)),
    Rule(18, Value(0.042, Token(18)), parent(x1P, x0P),father(x1P, x0P)),
    Rule(19, Value(0.037, Token(19)), ancestor(x1P, x0P),parent(x1P, x0P)),
    Rule(20, Value(0.035, Token(20)), ancestor(x1P, x0P),ancestor(x1P, x0P)),
  )
// result/nlp/ancestor_6012.nlp
  val soup8: Set[Rule] = Set(
    Rule(1, Value(0.260, Token(1)), ancestor(x1P, x0P),mother(x1P, x0P)),
    Rule(2, Value(0.248, Token(2)), ancestor(x1P, x0P),father(x1P, x0P)),
    Rule(3, Value(0.218, Token(3)), parent(x1P, x0P),father(x1P, x0P)),
    Rule(4, Value(0.206, Token(4)), parent(x1P, x0P),mother(x1P, x0P)),
    Rule(5, Value(0.186, Token(5)), ancestor(x1P, x0P),father(x1P, x0P)),
    Rule(6, Value(0.176, Token(6)), ancestor(x1P, x0P),mother(x1P, x0P)),
    Rule(7, Value(0.157, Token(7)), parent(x1P, x0P),father(x1P, x0P)),
    Rule(8, Value(0.146, Token(8)), parent(x1P, x0P),parent(x1P, x0P)),
    Rule(9, Value(0.142, Token(9)), parent(x1P, x0P),ancestor(x1P, x0P)),
    Rule(10, Value(0.143, Token(10)), ancestor(x1P, x0P),ancestor(x1P, x0P)),
    Rule(11, Value(0.133, Token(11)), parent(x1P, x0P),mother(x1P, x0P)),
    Rule(12, Value(0.117, Token(12)), ancestor(x1P, x0P),parent(x1P, x0P)),
    Rule(13, Value(0.070, Token(13)), parent(x1P, x0P),parent(x1P, x0P)),
    Rule(14, Value(0.065, Token(14)), parent(x1P, x0P),ancestor(x1P, x0P)),
    Rule(15, Value(0.059, Token(15)), ancestor(x1P, x0P),ancestor(x1P, x0P)),
    Rule(16, Value(0.048, Token(16)), ancestor(x2P, x0P),mother(x1P, x0P), father(x2P, x1P)),
    Rule(17, Value(0.046, Token(17)), ancestor(x2P, x0P),mother(x1P, x0P), mother(x2P, x1P)),
    Rule(18, Value(0.046, Token(18)), ancestor(x2P, x0P),father(x1P, x0P), father(x2P, x1P)),
    Rule(19, Value(0.044, Token(19)), ancestor(x2P, x0P),father(x1P, x0P), mother(x2P, x1P)),
    Rule(20, Value(0.042, Token(20)), ancestor(x1P, x0P),parent(x1P, x0P)),
    Rule(21, Value(0.035, Token(21)), parent(x2P, x0P),father(x1P, x0P), father(x2P, x1P)),
    Rule(22, Value(0.033, Token(22)), parent(x2P, x0P),mother(x1P, x0P), father(x2P, x1P)),
  )
// result/nlp/ancestor_68.nlp
  val soup9: Set[Rule] = Set(
    Rule(1, Value(0.267, Token(1)), ancestor(x1P, x0P),father(x1P, x0P)),
    Rule(2, Value(0.248, Token(2)), parent(x1P, x0P),mother(x1P, x0P)),
    Rule(3, Value(0.249, Token(3)), ancestor(x1P, x0P),mother(x1P, x0P)),
    Rule(4, Value(0.197, Token(4)), parent(x1P, x0P),mother(x1P, x0P)),
    Rule(5, Value(0.193, Token(5)), ancestor(x1P, x0P),father(x1P, x0P)),
    Rule(6, Value(0.177, Token(6)), ancestor(x1P, x0P),mother(x1P, x0P)),
    Rule(7, Value(0.170, Token(7)), parent(x1P, x0P),parent(x1P, x0P)),
    Rule(8, Value(0.141, Token(8)), parent(x1P, x0P),father(x1P, x0P)),
    Rule(9, Value(0.116, Token(9)), ancestor(x1P, x0P),parent(x1P, x0P)),
    Rule(10, Value(0.099, Token(10)), parent(x1P, x0P),ancestor(x1P, x0P)),
    Rule(11, Value(0.096, Token(11)), ancestor(x1P, x0P),ancestor(x1P, x0P)),
    Rule(12, Value(0.085, Token(12)), parent(x1P, x0P),parent(x1P, x0P)),
    Rule(13, Value(0.067, Token(13)), parent(x1P, x0P),father(x1P, x0P)),
    Rule(14, Value(0.052, Token(14)), ancestor(x2P, x0P),father(x1P, x0P), father(x2P, x1P)),
    Rule(15, Value(0.049, Token(15)), parent(x2P, x0P),mother(x1P, x0P), mother(x2P, x1P)),
    Rule(16, Value(0.048, Token(16)), ancestor(x2P, x0P),mother(x1P, x0P), father(x2P, x1P)),
    Rule(17, Value(0.047, Token(17)), ancestor(x2P, x0P),father(x1P, x0P), mother(x2P, x1P)),
    Rule(18, Value(0.044, Token(18)), ancestor(x2P, x0P),mother(x1P, x0P), mother(x2P, x1P)),
    Rule(19, Value(0.035, Token(19)), ancestor(x1P, x0P),parent(x1P, x0P)),
    Rule(20, Value(0.034, Token(20)), parent(x2P, x0P),parent(x1P, x0P), mother(x2P, x1P)),
    Rule(21, Value(0.034, Token(21)), parent(x1P, x0P),ancestor(x1P, x0P)),
  )
// result/nlp/ancestor_991.nlp
  val soup10: Set[Rule] = Set(
    Rule(1, Value(0.411, Token(1)), ancestor(x1P, x0P),mother(x1P, x0P)),
    Rule(2, Value(0.373, Token(2)), ancestor(x1P, x0P),mother(x1P, x0P)),
    Rule(3, Value(0.264, Token(3)), parent(x1P, x0P),mother(x1P, x0P)),
    Rule(4, Value(0.201, Token(4)), parent(x1P, x0P),mother(x1P, x0P)),
    Rule(5, Value(0.172, Token(5)), parent(x1P, x0P),father(x1P, x0P)),
    Rule(6, Value(0.149, Token(6)), parent(x1P, x0P),parent(x1P, x0P)),
    Rule(7, Value(0.153, Token(7)), ancestor(x2P, x0P),mother(x1P, x0P), mother(x2P, x1P)),
    Rule(8, Value(0.141, Token(8)), ancestor(x1P, x0P),ancestor(x1P, x0P)),
    Rule(9, Value(0.135, Token(9)), ancestor(x1P, x0P),father(x1P, x0P)),
    Rule(10, Value(0.100, Token(10)), parent(x1P, x0P),father(x1P, x0P)),
    Rule(11, Value(0.094, Token(11)), parent(x1P, x0P),ancestor(x1P, x0P)),
    Rule(12, Value(0.083, Token(12)), ancestor(x1P, x0P),parent(x1P, x0P)),
    Rule(13, Value(0.072, Token(13)), parent(x1P, x0P),parent(x1P, x0P)),
    Rule(14, Value(0.053, Token(14)), parent(x2P, x0P),mother(x1P, x0P), mother(x2P, x1P)),
    Rule(15, Value(0.053, Token(15)), ancestor(x2P, x0P),ancestor(x1P, x0P), mother(x2P, x1P)),
    Rule(16, Value(0.050, Token(16)), ancestor(x2P, x0P),father(x1P, x0P), mother(x2P, x1P)),
    Rule(17, Value(0.044, Token(17)), ancestor(x1P, x0P),ancestor(x1P, x0P)),
    Rule(18, Value(0.043, Token(18)), ancestor(x1P, x0P),father(x1P, x0P)),
    Rule(19, Value(0.035, Token(19)), parent(x2P, x0P),father(x1P, x0P), mother(x2P, x1P)),
  )
  val expected = Set(1,2,19,23)
  val maxVarCount: Int = 20

  val soupProg: Program = Program("EscapeSoup", soup)
  val soupProg1: Program = Program("EscapeSoup", soup1)
  val soupProg2: Program = Program("EscapeSoup", soup2)
  val soupProg3: Program = Program("EscapeSoup", soup3)
  val soupProg4: Program = Program("EscapeSoup", soup4)
  val soupProg5: Program = Program("EscapeSoup", soup5)
  val soupProg6: Program = Program("EscapeSoup", soup6)
  val soupProg7: Program = Program("EscapeSoup", soup7)
  val soupProg8: Program = Program("EscapeSoup", soup8)
  val soupProg9: Program = Program("EscapeSoup", soup9)
  val soupProg10: Program = Program("EscapeSoup", soup10)

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
