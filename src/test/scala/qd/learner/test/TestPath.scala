package qd
package learner
import org.scalatest.{FunSuite, Ignore}

class TestPath extends Problem {
	override val name = "path"
	val VSet = Range(0, 7).map(i => Atom(i)).toSet
	val V = Domain("V", VSet)
	val edge = Relation("edge", V,V)
	val path = Relation("path", V,V)
	val pathTuples = Set((0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7),(4,2),(4,3),(4,4),(4,5),(4,6),(4,7),(5,2),(5,3),(5,4),(5,5),(5,6),(5,7),(6,2),(6,3),(6,4),(6,5),(6,6),(6,7)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	val edgeTuples = Set((0,1),(1,2),(2,3),(2,7),(3,4),(4,5),(5,6),(6,3),(4,2)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
	override val edb = Config(
		edge -> (Instance(edge) ++ edgeTuples.map(t => t -> One).toMap),
		)
	override val refOut = Config(
		path -> (Instance(path) ++ pathTuples.map(t => t -> One).toMap),
		)
	val x1V = Variable("x1V",V)
	val x0V = Variable("x0V",V)
	val x2V = Variable("x2V",V)
	val soup = Set(
		Rule(0, Value(0.990000, Token(0)), path(x0V,x1V),edge(x0V,x1V)),
		Rule(1, Value(0.368783, Token(1)), path(x0V,x1V),edge(x0V,x1V),path(x0V,x2V)),
		Rule(2, Value(0.010000, Token(2)), path(x1V,x0V),edge(x0V,x2V),path(x0V,x1V)),
		Rule(3, Value(0.010000, Token(3)), path(x1V,x0V),edge(x0V,x1V),path(x0V,x2V)),
		Rule(4, Value(0.142155, Token(4)), path(x1V,x2V),path(x0V,x1V),path(x0V,x2V)),
		Rule(5, Value(0.010000, Token(5)), path(x1V,x2V),edge(x0V,x2V),path(x0V,x1V)),
		Rule(6, Value(0.010000, Token(6)), path(x1V,x2V),edge(x0V,x1V),path(x0V,x2V)),
		Rule(7, Value(0.010000, Token(7)), path(x1V,x2V),edge(x0V,x1V),edge(x0V,x2V)),
		Rule(8, Value(0.386567, Token(8)), path(x0V,x1V),edge(x0V,x1V),path(x2V,x0V)),
		Rule(9, Value(0.177378, Token(9)), path(x0V,x1V),edge(x0V,x1V),edge(x2V,x0V)),
		Rule(10, Value(0.833846, Token(10)), path(x2V,x0V),edge(x2V,x0V),path(x0V,x1V)),
		Rule(11, Value(0.599779, Token(11)), path(x2V,x0V),edge(x0V,x1V),edge(x2V,x0V)),
		Rule(12, Value(0.719907, Token(12)), path(x2V,x1V),path(x0V,x1V),path(x2V,x0V)),
		Rule(13, Value(0.990000, Token(13)), path(x2V,x1V),edge(x2V,x0V),path(x0V,x1V)),
		Rule(14, Value(0.975911, Token(14)), path(x2V,x1V),edge(x0V,x1V),path(x2V,x0V)),
		Rule(15, Value(0.990000, Token(15)), path(x2V,x1V),edge(x0V,x1V),edge(x2V,x0V)),
		)
	override val expected = Set(0,13)
	override val maxVarCount: Int = 20
}
