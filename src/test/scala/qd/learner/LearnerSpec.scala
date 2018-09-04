package qd
package learner

import java.nio.file.{Path, Paths}

import org.scalatest.FunSuite
import qd.data.graphs.Graphs

import scala.io.Source
import scala.util.Random

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

  test("Should be able to learn transitive closure from Line(5)") {
    val problem = parser.parseAll(parser.problem, simpleInput).get
    val tgtLoss = 0.1
    val maxIters = 100

    val learner = new Learner(problem)
    val result = learner.learn(tgtLoss, maxIters)
    assert(result._4 < tgtLoss)
  }

  for (graph <- Set(Graphs.line(8), Graphs.erdosRenyi(10, 0.1, 0))) {
    test(s"Should be able to learn transitive closure from ${graph.name}") {
      val maxLiterals = 2
      val maxVars = 3
      val tgtLoss = 0.1
      val maxIters = 1000

      val p0 = Problem().addInputRel(edge).addOutputRel(path)
                        .addEDBTuples(graph.edgeSet.map({ case (u, v) => (edge, DTuple(u, v)) }).toSeq:_*)
                        .addIDBTuples(graph.reachable.map({ case (u, v) => (path, DTuple(u, v)) }).toSeq:_*)

      var numTokens = 0
      def nextToken(): Token = { val ans = numTokens; numTokens = numTokens + 1; Token(s"R$ans") }
      val rng = Random
      def weight(l: Literal, ls: Set[Literal]): (Token, FValue) = {
        val token = nextToken()
        val value = FValue(rng.nextDouble(), token)
        (token, value)
      }

      val skeleton = Program.skeleton[FValue](p0.inputRels, p0.inventedRels, p0.outputRels,
                                              weight, maxLiterals, maxVars)
      val pos = TokenVec(skeleton._1.mapValues(_.v))
      val rules = skeleton._2

      val p1 = pos.foldLeft(p0) { case (p, (token, value)) => p.addToken(token, value) }
      val p2 = p1.addRules(rules)

      val learner = new Learner(p2)
      val result = learner.learn(tgtLoss, maxIters)
      // result._2.rules.toSeq.filter(_.coeff.v > 0.9).sortBy(-_.coeff.v).foreach(println)
      assert(result._4 < tgtLoss)
    }
  }

  /* for (graph <- Set(Graphs.line(8), Graphs.erdosRenyi(10, 0.15, 0)); if graph.components.nonEmpty) {
    test(s"Should be able to learn SCCs from ${graph.name}") {
      val maxLiterals = 2
      val maxVars = 3
      val tgtLoss = 0.1
      val maxIters = 1000

      val p0 = Problem().addInputRel(edge).addInventedRel(path).addOutputRel(scc)
                        .addEDBTuples(graph.edgeSet.map({ case (u, v) => (edge, DTuple(u, v)) }).toSeq:_*)
                        .addIDBTuples(graph.components.map({ case (u, v) => (path, DTuple(u, v)) }).toSeq:_*)

      var numTokens = 0
      def nextToken(): Token = { val ans = numTokens; numTokens = numTokens + 1; Token(s"R$ans") }
      val rng = Random
      def weight(l: Literal, ls: Set[Literal]): (Token, FValue) = {
        val token = nextToken()
        val value = FValue(rng.nextDouble(), token)
        (token, value)
      }

      val skeleton = Program.skeleton[FValue](p0.inputRels, p0.inventedRels, p0.outputRels,
                                              weight, maxLiterals, maxVars)
      val pos = TokenVec(skeleton._1.mapValues(_.v))
      val rules = skeleton._2

      val p1 = pos.foldLeft(p0) { case (p, (token, value)) => p.addToken(token, value) }
      val p2 = p1.addRules(rules)

      println(s"!!!!!!!! SCCs: ${graph.components}")
      val learner = new Learner(p2)
      val result = learner.learn(tgtLoss, maxIters)
      // result._2.rules.toSeq.filter(_.coeff.v > 0.9).sortBy(-_.coeff.v).foreach(println)
      assert(result._4 < tgtLoss)
    }
  } */

  /* val andersenInputLoc: String = "src/test/resources/Andersen-Train.qd"
  val andersenInput: String = Source.fromFile(andersenInputLoc).mkString

  test("Should be able to learn Andersen's analysis from NIPS data") {
    val problem = parser.parseAll(parser.problem, andersenInput).get
    val tgtLoss = 0.1
    val maxIters = 100

    println(s"!!!!!! Andersen numRules: ${problem.rules.size}")
    val learner = new Learner(problem)
    val result = learner.learn(tgtLoss, maxIters)
    result._2.rules.toSeq.filter(_.coeff.v > 0.9).sortBy(-_.coeff.v).foreach(println)
    assert(result._4 < tgtLoss)
    learner.reinterpret
  } */

}
