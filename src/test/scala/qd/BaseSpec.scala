package qd

import org.scalatest.FunSuite
import qd.data.graphs.Graphs._

class BaseSpec extends FunSuite {

  val vs: Semiring[FValue] = implicitly[Semiring[FValue]]

  val u = Variable("u", node)
  val v = Variable("v", node)
  val w = Variable("w", node)
  val x = Variable("x", node)
  val y = Variable("y", node)
  val z = Variable("z", node)

  test("Valencies should be correctly computed") {
    val rule1 = Rule(vs.Zero, Literal(path, Vector(u, v)),
                              Vector(Literal(edge, Vector(u, v))))
    assert(rule1.valency == 2)

    val rule2 = Rule(vs.Zero, Literal(path, Vector(u, w)),
                              Vector(Literal(edge, Vector(u, v)),
                                     Literal(edge, Vector(v, w))))
    assert(rule2.valency == 2)

    val rule3 = Rule(vs.Zero, Literal(path, Vector(u, w)),
                              Vector(Literal(edge, Vector(v, w)),
                                     Literal(edge, Vector(u, v))))
    assert(rule3.valency == 2)

    val rule4 = Rule(vs.Zero, Literal(path, Vector(u, x)),
                              Vector(Literal(edge, Vector(u, v)),
                                     Literal(edge, Vector(w, x)),
                                     Literal(edge, Vector(v, w))))
    assert(rule4.valency == 4)

    val rule5 = rule4.normalized
    assert(rule5.valency == 2)
  }

}
