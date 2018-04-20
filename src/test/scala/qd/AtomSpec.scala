package qd

import org.scalatest.{FlatSpec, Matchers}

class AtomSpec extends FlatSpec with Matchers {

  "Atoms" should "respect equality" in {
    val a51 = Atom(5)
    val a52 = Atom(5)
    val a61 = Atom(6)
    val a62 = Atom(6)

    val aa51 = Atom(a51)
    val aa52 = Atom(a52)
    val aa61 = Atom(a61)
    val aa62 = Atom(a62)

    assert(a51 == a51)
    assert(a51 == a52)
    assert(a51 != a61)
    assert(a51 != a62)
    assert(a51 != aa51)
    assert(a51 != aa52)
    assert(a51 != aa61)
    assert(a51 != aa62)

    assert(a52 == a51)
    assert(a52 == a52)
    assert(a52 != a61)
    assert(a52 != a62)
    assert(a52 != aa51)
    assert(a52 != aa52)
    assert(a52 != aa61)
    assert(a52 != aa62)

    assert(a61 != a51)
    assert(a61 != a52)
    assert(a61 == a61)
    assert(a61 == a62)
    assert(a61 != aa51)
    assert(a61 != aa52)
    assert(a61 != aa61)
    assert(a61 != aa62)

    assert(a62 != a51)
    assert(a62 != a52)
    assert(a62 == a61)
    assert(a62 == a62)
    assert(a62 != aa51)
    assert(a62 != aa52)
    assert(a62 != aa61)
    assert(a62 != aa62)

    assert(aa51 != a51)
    assert(aa51 != a52)
    assert(aa51 != a61)
    assert(aa51 != a62)
    assert(aa51 == aa51)
    assert(aa51 == aa52)
    assert(aa51 != aa61)
    assert(aa51 != aa62)

    assert(aa52 != a51)
    assert(aa52 != a52)
    assert(aa52 != a61)
    assert(aa52 != a62)
    assert(aa52 == aa51)
    assert(aa52 == aa52)
    assert(aa52 != aa61)
    assert(aa52 != aa62)

    assert(aa61 != a51)
    assert(aa61 != a52)
    assert(aa61 != a61)
    assert(aa61 != a62)
    assert(aa61 != aa51)
    assert(aa61 != aa52)
    assert(aa61 == aa61)
    assert(aa61 == aa62)

    assert(aa62 != a51)
    assert(aa62 != a52)
    assert(aa62 != a61)
    assert(aa62 != a62)
    assert(aa62 != aa51)
    assert(aa62 != aa52)
    assert(aa62 == aa61)
    assert(aa62 == aa62)
  }

  val triangle = Atom("triangle")
  val rectangle = Atom("rectangle")
  val circle = Atom("circle")
  val shapes = Domain("Shape", triangle, rectangle, circle)

  "Atoms and domains" should "print nicely" in {
    assert(triangle.toString == "<triangle>")
    assert(rectangle.toString == "<rectangle>")
    assert(circle.toString == "<circle>")
    assert(shapes.toString == "Shape[<triangle>, <rectangle>, <circle>]")
  }

}
