package qd

import org.scalatest.FunSuite
import qd.data.graphs.Graphs

class ProblemParserSpec extends FunSuite {

  val parser = new ProblemParser

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
    val result = parser.parseAll(parser.problem, simpleInput1)
    assert(result.successful)
    val state = result.get

    assert(state.inputRels == Set(edge, nullRel))
    assert(state.inventedRels == Set(path))
    assert(state.outputRels == Set(scc))

    assert(state.edb == Set((edge, DTuple(a, b)), (edge, DTuple(b, c)), (edge, DTuple(c, d)), (edge, DTuple(a, c))))
    assert(state.idb == Set((path, DTuple(a, b)), (path, DTuple(b, c))))

    assert(state.rules.size == 2)
    val nullRule = state.rules.find(rule => rule.head.relation == nullRel && rule.body.isEmpty).get
    assert(nullRule.coeff.v == 0.2)
  }

  val simpleInput2: String = """Input { edge(Node, Node) }
                               |Invented { path(Node, Node) }
                               |Output { scc(Node, Node) }
                               |AllRules(3, 4)""".stripMargin

  test("Should parse simpleInput2") {
    val result = parser.parseAll(parser.problem, simpleInput2)
    assert(result.successful)
    val state = result.get

    assert(state.inputRels == Set(edge))
    assert(state.inventedRels == Set(path))
    assert(state.outputRels == Set(scc))

    assert(state.edb.isEmpty)
    assert(state.idb.isEmpty)

    assert(state.rules.size == 21443)
  }

  val simpleInput3: String = """Input { edge(Node, Node) }
                               |Invented { path(Node, Node) }
                               |Output { scc(Node, Node) }
                               |AllRules(3, 4, 0.2)""".stripMargin

  test("Should parse simpleInput3") {
    val result = parser.parseAll(parser.problem, simpleInput3)
    assert(result.successful)
    val state = result.get

    assert(state.inputRels == Set(edge))
    assert(state.inventedRels == Set(path))
    assert(state.outputRels == Set(scc))

    assert(state.edb.isEmpty)
    assert(state.idb.isEmpty)

    assert(state.rules.size == 21443)
    assert(state.rules.forall(_.coeff.v == 0.2))
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
    val result = parser.parseAll(parser.problem, commentedInput)
    assert(result.successful)
    val state = result.get

    assert(state.inputRels == Set(edge, Relation("null")))
    assert(state.inventedRels == Set(path))
    assert(state.outputRels == Set(scc))

    assert(state.edb == Set((edge, DTuple(a, b)), (edge, DTuple(b, c)), (edge, DTuple(c, d)), (edge, DTuple(a, c))))
    assert(state.idb == Set((path, DTuple(a, b)), (path, DTuple(b, c))))

    assert(state.rules.isEmpty)
  }

  val badInput1: String = """Input { edge(Node, Node),
                            |Invented { path(Node, Node) }
                            |Output { scc(Node, Node) }
                            |EDB { edge(a, b), edge(b, c), edge(c, d), edge(a, c) }
                            |IDB { path(a, b), path(b, c) }""".stripMargin

  test("Should fail to parse badInput1") {
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
      parser.parseAll(parser.problem, badInput2)
      fail()
    } catch { case _: Throwable => () }
  }

}