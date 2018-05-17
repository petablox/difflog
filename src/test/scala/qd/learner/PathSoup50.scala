package qd
package learner
import org.scalatest.{FunSuite, Ignore}

class PathSoup50 extends Problem {
	override val name = "path"
	val VSet = Range(0, 7).map(i => Atom(i)).toSet
	val V = Domain("V", VSet)
	val edge = Relation("edge", V,V)
	val path = Relation("path", V,V)
	val pathTuples = Set((1, 2),(2, 3),(3, 4),(3, 5),(6, 4),(1, 3),(1, 4),(1, 5),(2, 4),(2, 5)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
  val edgeTuples = Set((1, 2),(2, 3),(3, 4),(3, 5),(6, 4)).map { case (x0,x1) => DTuple(Atom(x0),Atom(x1)) }
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
    Rule(0, Value(0.5, Token(0)), path(x0V,x1V),edge(x0V,x1V)),
    Rule(1, Value(0.5, Token(1)), path(x0V,x1V),edge(x0V,x1V),path(x0V,x2V)),
    Rule(3, Value(0.5, Token(3)), path(x1V,x0V),edge(x0V,x1V),path(x0V,x2V)),
    Rule(5, Value(0.5, Token(5)), path(x1V,x2V),edge(x0V,x2V),path(x0V,x1V)),
    Rule(8, Value(0.5, Token(8)), path(x0V,x1V),edge(x0V,x1V),path(x2V,x0V)),
    Rule(9, Value(0.5, Token(9)), path(x0V,x1V),edge(x0V,x1V),edge(x2V,x0V)),
    Rule(12, Value(0.5, Token(12)), path(x2V,x1V),path(x0V,x1V),path(x2V,x0V)),
    Rule(13, Value(0.5, Token(13)), path(x2V,x1V),edge(x2V,x0V),path(x0V,x1V)),
    )
	override val expected = Set(0,13)
	override val maxVarCount: Int = 20
}
