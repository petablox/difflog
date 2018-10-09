package qd
package learner

import org.scalatest.FunSuite
import qd.data.graphs.Graphs
import qd.problem.{Problem, QDParser}

class LearnerSpec extends FunSuite {

  val parser = new QDParser

  val node: Domain = Graphs.node
  val edge: Relation = Graphs.edge
  val nullRel: Relation = Relation("null", Vector())
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
  val problem: Problem = parser.parseAll(parser.problem, simpleInput).get

  test(s"Should be able to learn transitive closure from Line(5) and ${problem.rules.size} rules") {
    val tgtLoss = 0.1
    val maxIters = 100

    val learner = new Learner(problem, L2Scorer)
    val result = learner.learn(tgtLoss, maxIters)
    assert(result._4 < tgtLoss)
  }

}
