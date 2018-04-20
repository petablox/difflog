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
      if (random.nextDouble() < 0.1) {
        allEdges = allEdges + edge(Atom(i), Atom(j))
      }
    }
  }
  val edb = Config(Instance(edge, allEdges))
  val startTime: Long = System.nanoTime()
  val idbNaive: Config = NaiveEvaluator(program)(edb)
  val naiveTime: Long = System.nanoTime() - startTime
  val idbSeminaive: Config = SeminaiveEvaluator(program)(edb)
  val seminaiveTime: Long = System.nanoTime() - naiveTime - startTime

  println(s"naiveTime: ${naiveTime / 1.0e9} seminaiveTime: ${seminaiveTime / 1.0e9}")
  println(s"idbNaive.numTuples: ${idbNaive.numTuples}, idbNaive(path).numTuples: ${idbNaive(path).numTuples}")
  println(s"idbSeminaive.numTuples: ${idbSeminaive.numTuples}, idbSemiNaive(path).numTuples: ${idbSeminaive(path).numTuples}")
  // println(idb)

  "Naive and Seminaive evaluation" should "coincide" in {
    assert(idbNaive == idbSeminaive)
  }

}
