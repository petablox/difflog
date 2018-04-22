package qd

import org.scalatest.FunSuite

import scala.util.Random

class PathEdge extends FunSuite {

  val ALL_GRAPHS: Set[Graph] = Set(smallGraph,
                                   line(1), line(2), line(10),
                                   erdosRenyi(50, 0.1, 0), erdosRenyi(100, 0.01, 0))
  val ALL_PROGRAMS: Set[Graph => Program] = Set(PE, EP, PP)
  val ALL_EVALUATORS: Set[Program => Evaluator] = Set(NaiveEvaluator, SeminaiveEvaluator)

  for (graph <- ALL_GRAPHS; gp <- ALL_PROGRAMS; pe <- ALL_EVALUATORS) {
    val program = gp(graph)
    val evaluator = pe(program)
    val edb = Config(Instance(graph.edge, graph.edgeSet.map { case (from, to) => DTuple(from, to) }))

    test(s"Applying evaluator ${evaluator.name} " +
         s"to program ${program.name} and graph ${graph.name}") {
      val startTime = System.nanoTime()
      val idb = evaluator(edb)
      val endTime = System.nanoTime()
      val produced = idb(graph.path)
      assert(produced.forall(_.length == 2))
      assert(produced.map(t => (t(0), t(1))) == graph.reachable)
      // println(s"${graph.name}, ${program.name}, ${evaluator.name}: ${(endTime - startTime) / 1.0e9}")
    }
  }

  case class Graph(name: Any, nodeSet: Set[Atom], edgeSet: Set[(Atom, Atom)]) {
    require(nodeSet.nonEmpty)
    require(edgeSet.forall { case (from, to) => nodeSet(from) && nodeSet(to) })

    val nodes: Domain = Domain("Node", nodeSet)
    val edge = Relation("edge", nodes, nodes)
    val path = Relation("path", nodes, nodes)

    val reachable: Set[(Atom, Atom)] = reachable(nodeSet.size + 1)

    def reachable(steps: Int): Set[(Atom, Atom)] = {
      require(steps >= 1)
      if (steps == 1) edgeSet
      else {
        val rn1 = reachable(steps - 1)
        val r = for ((i, j) <- rn1; (k, l) <- edgeSet; if j == k) yield (i, l)
        rn1 ++ r
      }
    }
  }

  def smallGraph: Graph = {
    val node1 = Atom(1)
    val node2 = Atom(2)
    val node3 = Atom(3)
    val nodeSet = Set(node1, node2, node3)
    val edgeSet = Set((node1, node2), (node2, node2), (node2, node3))
    Graph("SmallGraph", nodeSet, edgeSet)
  }

  def line(n: Int): Graph = {
    require(n > 0)
    val nodeSet = Range(0, n).map(Atom).toSet
    val edgeSet = Range(0, n - 1).map(i => (Atom(i), Atom(i + 1))).toSet
    Graph(s"Line($n)", nodeSet, edgeSet)
  }

  def erdosRenyi(n: Int, p: Double, seed: Int): Graph = {
    require(n > 0 && 0.0 <= p && p <= 1.0)
    val random = new Random(seed)
    val nodeSet = Range(0, n).map(Atom).toSet
    val edgeSet = for (i <- nodeSet; j <- nodeSet; if random.nextDouble() < p) yield (i, j)
    Graph(s"Erdos-Renyi($n, $p, $seed)", nodeSet, edgeSet)
  }

  def PE(graph: Graph): Program = {
    val nodes = graph.nodes
    val edge = graph.edge
    val path = graph.path

    val x = Variable("x", nodes)
    val y = Variable("y", nodes)
    val z = Variable("z", nodes)
    val ruleE = Rule("E", path(x, y), edge(x, y))
    val ruleT = Rule("T", path(x, z), path(x, y), edge(y, z))

    Program("PE", ruleE, ruleT)
  }

  def EP(graph: Graph): Program = {
    val nodes = graph.nodes
    val edge = graph.edge
    val path = graph.path

    val x = Variable("x", nodes)
    val y = Variable("y", nodes)
    val z = Variable("z", nodes)
    val ruleE = Rule("E", path(x, y), edge(x, y))
    val ruleT = Rule("T", path(x, z), edge(x, y), path(y, z))

    Program("EP", ruleE, ruleT)
  }

  def PP(graph: Graph): Program = {
    val nodes = graph.nodes
    val edge = graph.edge
    val path = graph.path

    val x = Variable("x", nodes)
    val y = Variable("y", nodes)
    val z = Variable("z", nodes)
    val ruleE = Rule("E", path(x, y), edge(x, y))
    val ruleT = Rule("T", path(x, z), path(x, y), path(y, z))

    Program("PP", ruleE, ruleT)
  }

}