package qd
package learner

import jdk.nashorn.internal.ir.annotations.Ignore

@Ignore
class TestAncestor extends Problem {
	override val name = "ancestor"
	val PSet = Range(0, 8).map(i => Atom(i)).toSet
	val P = Domain("P", PSet)
	val father = Relation("father", P,P)
	val mother = Relation("mother", P,P)
	val parent = Relation("parent", P,P)
	val ancestor = Relation("ancestor", P,P)
	val ancestorTuples = Set((6, 7), (0, 7), (0, 6), (3, 6), (3, 7), (0, 3)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val fatherTuples = Set((3, 7), (0, 3), (3, 6)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val parentTuples = Set((3, 7), (0, 3), (3, 6),(6,7)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val motherTuples = Set((6,7)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	override val edb = Config(
		father -> (Instance(father) ++ fatherTuples.map(t => t -> One).toMap),
		mother -> (Instance(mother) ++ motherTuples.map(t => t -> One).toMap),
		)
	override val refOut = Config(
		parent -> (Instance(parent) ++ parentTuples.map(t => t -> One).toMap),
		ancestor -> (Instance(ancestor) ++ ancestorTuples.map(t => t -> One).toMap),
		)
	val x2P = Variable("x2P",P)
	val x0P = Variable("x0P",P)
	val x1P = Variable("x1P",P)
	val soup_pre = Set(
		Rule(1, Value(0.619612, Token(1)), parent(x0P,x1P),ancestor(x0P,x1P)),
		Rule(2, Value(0.990000, Token(2)), parent(x0P,x1P),father(x0P,x1P)),
		Rule(3, Value(0.990000, Token(3)), parent(x0P,x1P),mother(x0P,x1P)),
		Rule(4, Value(0.189379, Token(4)), parent(x2P,x1P),ancestor(x0P,x1P),ancestor(x2P,x0P)),
		Rule(5, Value(0.143807, Token(5)), parent(x2P,x1P),ancestor(x0P,x1P),parent(x2P,x0P)),
		Rule(6, Value(0.545348, Token(6)), parent(x2P,x1P),ancestor(x0P,x1P),father(x2P,x0P)),
		Rule(7, Value(0.499857, Token(7)), parent(x2P,x1P),ancestor(x0P,x1P),mother(x2P,x0P)),
		Rule(8, Value(0.346683, Token(8)), parent(x2P,x1P),ancestor(x2P,x0P),parent(x0P,x1P)),
		Rule(9, Value(0.990000, Token(9)), parent(x2P,x1P),parent(x0P,x1P),parent(x2P,x0P)),
		Rule(10, Value(0.819182, Token(10)), parent(x2P,x1P),father(x2P,x0P),parent(x0P,x1P)),
		Rule(11, Value(0.567587, Token(11)), parent(x2P,x1P),mother(x2P,x0P),parent(x0P,x1P)),
		Rule(12, Value(0.990000, Token(12)), parent(x2P,x1P),ancestor(x2P,x0P),father(x0P,x1P)),
		Rule(13, Value(0.286935, Token(13)), parent(x2P,x1P),father(x0P,x1P),parent(x2P,x0P)),
		Rule(14, Value(0.662515, Token(14)), parent(x2P,x1P),father(x0P,x1P),father(x2P,x0P)),
		Rule(15, Value(0.990000, Token(15)), parent(x2P,x1P),father(x0P,x1P),mother(x2P,x0P)),
		Rule(16, Value(0.269031, Token(16)), parent(x2P,x1P),ancestor(x2P,x0P),mother(x0P,x1P)),
		Rule(17, Value(0.920557, Token(17)), parent(x2P,x1P),mother(x0P,x1P),parent(x2P,x0P)),
		Rule(18, Value(0.147241, Token(18)), parent(x2P,x1P),father(x2P,x0P),mother(x0P,x1P)),
		Rule(19, Value(0.160519, Token(19)), parent(x2P,x1P),mother(x0P,x1P),mother(x2P,x0P)),
		Rule(20, Value(0.010000, Token(20)), ancestor(x0P,x1P),parent(x0P,x1P)),
		Rule(21, Value(0.990000, Token(21)), ancestor(x0P,x1P),father(x0P,x1P)),
		Rule(22, Value(0.990000, Token(22)), ancestor(x0P,x1P),mother(x0P,x1P)),
		Rule(23, Value(0.010000, Token(23)), ancestor(x2P,x1P),ancestor(x0P,x1P),ancestor(x2P,x0P)),
		Rule(24, Value(0.010000, Token(24)), ancestor(x2P,x1P),ancestor(x0P,x1P),parent(x2P,x0P)),
		Rule(25, Value(0.015551, Token(25)), ancestor(x2P,x1P),ancestor(x0P,x1P),father(x2P,x0P)),
		Rule(26, Value(0.017446, Token(26)), ancestor(x2P,x1P),ancestor(x0P,x1P),mother(x2P,x0P)),
		Rule(27, Value(0.010000, Token(27)), ancestor(x2P,x1P),ancestor(x2P,x0P),parent(x0P,x1P)),
		Rule(28, Value(0.010000, Token(28)), ancestor(x2P,x1P),parent(x0P,x1P),parent(x2P,x0P)),
		Rule(29, Value(0.010000, Token(29)), ancestor(x2P,x1P),father(x2P,x0P),parent(x0P,x1P)),
		Rule(30, Value(0.010000, Token(30)), ancestor(x2P,x1P),mother(x2P,x0P),parent(x0P,x1P)),
		Rule(31, Value(0.014037, Token(31)), ancestor(x2P,x1P),ancestor(x2P,x0P),father(x0P,x1P)),
		Rule(32, Value(0.010000, Token(32)), ancestor(x2P,x1P),father(x0P,x1P),parent(x2P,x0P)),
		Rule(33, Value(0.292259, Token(33)), ancestor(x2P,x1P),father(x0P,x1P),father(x2P,x0P)),
		Rule(34, Value(0.019543, Token(34)), ancestor(x2P,x1P),father(x0P,x1P),mother(x2P,x0P)),
		Rule(35, Value(0.011762, Token(35)), ancestor(x2P,x1P),ancestor(x2P,x0P),mother(x0P,x1P)),
		Rule(36, Value(0.010000, Token(36)), ancestor(x2P,x1P),mother(x0P,x1P),parent(x2P,x0P)),
		Rule(37, Value(0.018947, Token(37)), ancestor(x2P,x1P),father(x2P,x0P),mother(x0P,x1P)),
		Rule(38, Value(0.017244, Token(38)), ancestor(x2P,x1P),mother(x0P,x1P),mother(x2P,x0P)),
		)

	override val expected = Set(1,2,19,23)
	override val maxVarCount: Int = 20

  val usefulTokens= Set(2, 3, 20, 27, 34, 37)
  val soup =
    soup_pre.map(r => Rule(r.name, Value(1.0, r.coeff.prov), r.head, r.body)).
    filter(r => usefulTokens.contains(r.name.asInstanceOf[Int]))
}
