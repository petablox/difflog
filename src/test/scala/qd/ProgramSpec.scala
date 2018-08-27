package qd

import org.scalatest.FunSuite
import graphs.Graphs

class ProgramSpec extends FunSuite {

  val edge: Relation = Graphs.edge
  val path: Relation = Graphs.path
  val scc: Relation = Graphs.scc

  test("Trivial") {
    val p = Program.skeleton[FValue]("P", Set(edge), Set(path), Set(scc), 3, 4)
    println(p.rules.size)
    p.rules.take(5).foreach(println)
    assert(true)
  }

}
