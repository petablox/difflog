package qd
package data
package graphs

import scala.util.Random

object Graphs {

  implicit val vs: Semiring[FValue] = Semiring.FValueSemiringObj

  val node: Domain = Domain("Node")
  val edge: Relation = Relation("edge", List(node, node))
  val path = Relation("path", List(node, node))
  val scc = Relation("scc", List(node, node))

  def vertex(name: Any): Constant = Constant(name, node)

  val Graphs: Set[Graph] = Set(smallGraph) ++
                           Range(1, 16, 2).map(line).toSet ++
                           Range(1, 16, 2).map(circle).toSet +
                           erdosRenyi(50, 0.1, 0) + erdosRenyi(100, 0.01, 0)

  case class Graph(name: Any, nodeSet: Set[Constant], edgeSet: Set[(Constant, Constant)]) {
    require(nodeSet.nonEmpty)
    require(edgeSet.forall { case (from, to) => nodeSet(from) && nodeSet(to) })

    def edb: Config[FValue] = Config(Map(edge -> edgeSet.map({ case (a, b) => DTuple(List(a, b)) -> vs.One})
                                                        .foldLeft(Instance[FValue](List(node, node)))(_ + _)))

    val reachable: Set[(Constant, Constant)] = reachable(nodeSet.size + 1)

    def reachable(steps: Int): Set[(Constant, Constant)] = {
      require(steps >= 1)
      if (steps == 1) edgeSet
      else {
        val rn1 = reachable(steps - 1)
        val r = for ((i, j) <- rn1; (k, l) <- edgeSet; if j == k) yield (i, l)
        rn1 ++ r
      }
    }

    val components: Set[(Constant, Constant)] = reachable.filter(p => reachable((p._2, p._1)))
  }

  def smallGraph: Graph = {
    val node1 = vertex(1)
    val node2 = vertex(2)
    val node3 = vertex(3)
    val nodeSet = Set(node1, node2, node3)
    val edgeSet = Set((node1, node2), (node2, node2), (node2, node3))
    Graph("SmallGraph", nodeSet, edgeSet)
  }

  def line(n: Int): Graph = {
    require(n > 0)
    val nodeSet = Range(0, n).map(i => vertex(i)).toSet
    val edgeSet = Range(0, n - 1).map(i => (vertex(i), vertex(i + 1))).toSet
    Graph(s"Line($n)", nodeSet, edgeSet)
  }

  def circle(n: Int): Graph = {
    require(n > 0)
    val nodeSet = Range(0, n).map(i => vertex(i)).toSet
    val edgeSet = Range(0, n).map(i => (vertex(i), vertex((i + 1) % n))).toSet
    Graph(s"Circle($n)", nodeSet, edgeSet)
  }

  def erdosRenyi(n: Int, p: Double, seed: Int): Graph = {
    require(n > 0 && 0.0 <= p && p <= 1.0)
    val random = new Random(seed)
    val nodeSet = Range(0, n).map(i => vertex(i)).toSet
    val edgeSet = for (i <- nodeSet; j <- nodeSet; if random.nextDouble() < p) yield (i, j)
    Graph(s"Erdos-Renyi($n, $p, $seed)", nodeSet, edgeSet)
  }

}
