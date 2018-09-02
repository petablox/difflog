package qd

import org.scalatest.FunSuite
import qd.data.graphs.Graphs

class ParserSpec extends FunSuite {

  val node: Domain = Graphs.node
  val edge: Relation = Graphs.edge
  val nullRel: Relation = Relation("null")
  val path: Relation = Graphs.path
  val scc: Relation = Graphs.scc

  val a = Constant("a", node)
  val b = Constant("b", node)
  val c = Constant("c", node)
  val d = Constant("d", node)

  val simpleInput1: String = """Input { edge(Node, Node), null() }
                              |Invented { path(Node, Node) }
                              |Output { scc(Node, Node) }
                              |EDB { edge(a, b), edge(b, c), edge(c, d), edge(a, c) }
                              |IDB { path(a, b), path(b, c) }
                              |Rules{
                              |  path(v1, v2) :- edge(v2, v1).
                              |  0.2: null() :- .
                              |}""".stripMargin

  test("Should parse simpleInput1") {
    val parser = new Parser
    val result = parser.parseAll(parser.problem, simpleInput1)
    assert(result.successful)
    val problem = result.get

    assert(problem.inputRels == Set(edge, nullRel))
    assert(problem.inventedRels == Set(path))
    assert(problem.outputRels == Set(scc))

    assert(problem.edb == Set((edge, DTuple(a, b)), (edge, DTuple(b, c)), (edge, DTuple(c, d)), (edge, DTuple(a, c))))
    assert(problem.idb == Set((path, DTuple(a, b)), (path, DTuple(b, c))))

    assert(problem.rules.size == 2)
    val nullRule = problem.rules.find(rule => rule.head.relation == nullRel && rule.body.isEmpty).get
    assert(nullRule.coeff.v == 0.2)
  }

  val simpleInput2: String = """Input { edge(Node, Node) }
                               |Invented { path(Node, Node) }
                               |Output { scc(Node, Node) }
                               |AllRules(3, 4)""".stripMargin

  test("Should parse simpleInput2") {
    val parser = new Parser
    val result = parser.parseAll(parser.problem, simpleInput2)
    assert(result.successful)
    val problem = result.get

    assert(problem.inputRels == Set(edge))
    assert(problem.inventedRels == Set(path))
    assert(problem.outputRels == Set(scc))

    assert(problem.edb.isEmpty)
    assert(problem.idb.isEmpty)

    assert(problem.rules.size == 21443)
  }

  val simpleInput3: String = """Input { edge(Node, Node) }
                               |Invented { path(Node, Node) }
                               |Output { scc(Node, Node) }
                               |AllRules(3, 4, 0.2)""".stripMargin

  test("Should parse simpleInput3") {
    val parser = new Parser
    val result = parser.parseAll(parser.problem, simpleInput3)
    assert(result.successful)
    val problem = result.get

    assert(problem.inputRels == Set(edge))
    assert(problem.inventedRels == Set(path))
    assert(problem.outputRels == Set(scc))

    assert(problem.edb.isEmpty)
    assert(problem.idb.isEmpty)

    assert(problem.rules.size == 21443)
    assert(problem.rules.forall(_.coeff.v == 0.2))
  }

  val commentedInput: String = """Input { edge(Node, Node), null() }
                                 |Invented { /* path(Node, Node, Node) */ path(Node, Node) }
                                 |Invented { }
                                 |Invented { // } path(Node)
                                 |}
                                 |Output { scc(Node, Node) }
                                 |EDB { edge(a, b), edge(b, c), edge(c, d), edge(a, c) }
                                 |IDB { path(a, b), path(b, c) }""".stripMargin

  test("Should parse the commented input") {
    val parser = new Parser
    val result = parser.parseAll(parser.problem, commentedInput)
    assert(result.successful)
    val problem = result.get

    assert(problem.inputRels == Set(edge, Relation("null")))
    assert(problem.inventedRels == Set(path))
    assert(problem.outputRels == Set(scc))

    assert(problem.edb == Set((edge, DTuple(a, b)), (edge, DTuple(b, c)), (edge, DTuple(c, d)), (edge, DTuple(a, c))))
    assert(problem.idb == Set((path, DTuple(a, b)), (path, DTuple(b, c))))

    assert(problem.rules.isEmpty)
  }

  val badInput1: String = """Input { edge(Node, Node),
                            |Invented { path(Node, Node) }
                            |Output { scc(Node, Node) }
                            |EDB { edge(a, b), edge(b, c), edge(c, d), edge(a, c) }
                            |IDB { path(a, b), path(b, c) }""".stripMargin

  test("Should fail to parse badInput1") {
    val parser = new Parser
    val result = parser.parseAll(parser.problem, badInput1)
    assert(!result.successful)
  }

  val badInput2: String = """Input { edge(Node, Node), edge() }
                           |Invented { path(Node, Node) }
                           |Output { scc(Node, Node) }
                           |EDB { edge(a, b), edge(b, c), edge(c, d), edge(a, c) }
                           |IDB { path(a, b), path(b, c) }""".stripMargin

  test("Should fail to parse badInput2") {
    try {
      val parser = new Parser
      parser.parseAll(parser.problem, badInput2)
      fail()
    } catch { case _: Throwable => () }
  }

}
