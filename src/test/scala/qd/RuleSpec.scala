package qd

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class RuleSpec extends FlatSpec with Matchers {

  val node1 = Atom(1)
  val node2 = Atom(2)
  val node3 = Atom(3)
  // val nodes = Domain("Node", node1, node2, node3)
  val nodes = Domain("Node", Range(0, 100).map(Atom).toSet)

  val edge = Relation("edge", nodes, nodes)
  val path = Relation("path", nodes, nodes)

  val x = Variable("x", nodes)
  val y = Variable("y", nodes)
  val z = Variable("z", nodes)
  val ruleE = Rule("E", path(x, y), edge(x, y))
  val ruleT = Rule("T", path(x, z), path(x, y), edge(y, z))

  val program = Program(ruleE, ruleT)
  println(program)
  /* val edb = Config(Instance(edge, edge(node1, node2),
                                  edge(node2, node2),
                                  edge(node2, node3))) */
  var allEdges: Set[DTuple] = Set()
  val random = new Random()
  for (i <- Range(0, nodes.size)) {
    for (j <- Range(0, nodes.size)) {
      if (random.nextDouble() < 0.01) {
        allEdges = allEdges + edge(Atom(i), Atom(j))
      }
    }
  }
  val edb = Config(Instance(edge, allEdges))
  val idb = NaiveEvaluator(program)(edb)
  println(s"idb.numTuples: ${idb.numTuples}, id(path).numTuples: ${idb(path).numTuples}")
  // println(idb)

}
