package qd

import org.scalatest.FunSuite
import qd.Semiring.FValueSemiringObj
import qd.data.graphs.Graphs

class BaseSpec extends FunSuite {

  implicit val vs: FValueSemiring = FValueSemiringObj

  val node: Domain = Graphs.node
  val edge: Relation = Graphs.edge
  val path: Relation = Graphs.path

  val u = Variable("u", node)
  val v = Variable("v", node)
  val w = Variable("w", node)
  val x = Variable("x", node)
  val y = Variable("y", node)
  val z = Variable("z", node)

  test("Valencies should be correctly computed") {
    val rule1 = Rule(vs.Zero, Literal(path, List(u, v)),
                              List(Literal(edge, List(u, v))))
    assert(rule1.valency == 2)

    val rule2 = Rule(vs.Zero, Literal(path, List(u, w)),
                              List(Literal(edge, List(u, v)),
                                   Literal(edge, List(v, w))))
    assert(rule2.valency == 2)

    val rule3 = Rule(vs.Zero, Literal(path, List(u, w)),
                              List(Literal(edge, List(v, w)),
                                   Literal(edge, List(u, v))))
    assert(rule3.valency == 2)
  }

}
