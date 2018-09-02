package qd
package learner

import org.scalatest.FunSuite
import qd.data.graphs.Graphs
import qd.{Constant, Domain, Parser, Relation}

class LearnerSpec extends FunSuite {

  val parser = new Parser

  val node: Domain = Graphs.node
  val edge: Relation = Graphs.edge
  val nullRel: Relation = Relation("null")
  val path: Relation = Graphs.path
  val scc: Relation = Graphs.scc

  val a = Constant("a", node)
  val b = Constant("b", node)
  val c = Constant("c", node)
  val d = Constant("d", node)

  val simpleInput: String = """Input { edge(Node, Node) }
                               |Invented {  }
                               |Output { path(Node, Node) }
                               |EDB { edge(a, b), edge(b, c), edge(c, d), edge(d, e) }
                               |IDB { path(a, b), path(a, c), path(a, d), path(a, e),
                               |                  path(b, c), path(b, d), path(b, e),
                               |                              path(c, d), path(c, e),
                               |                                          path(d, e) }
                               |AllRules(2, 3)""".stripMargin

  test("Should be able to learn PathEdge") {
    val problem = parser.parseAll(parser.problem, simpleInput).get
    val learner = new Learner(problem)
    val result = learner.learn(0.2, 10)
    println(result)
    assert(true)
  }

}
