package qd

import org.scalatest.FunSuite

class ProblemParserSpec extends FunSuite {

  val parser = new ProblemParser

  val simpleInput: String = """Input { edge(Node, Node), null() }
                              |Invented { path(Node, Node) }
                              |Output { scc(Node, Node) }
                              |EDB { edge(a, b), edge(b, c), edge(c, d), edge(a, c) }
                              |IDB { path(a, b), path(b, c) }""".stripMargin

  test("Should parse the simple input") {
    val result = parser.parseAll(parser.problem, simpleInput)
    assert(result.successful)
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
