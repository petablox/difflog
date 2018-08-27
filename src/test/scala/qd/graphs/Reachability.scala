package qd
package graphs

import Graphs.Graph

object Reachability {

  val Programs: Set[Graph => Program[FValue]] = Set(PE, EP, PP)

  val ve: FValue = FValue(0.8, Token("E"))
  val vt: FValue = FValue(0.9, Token("T"))

  val nodes: Domain = Graphs.nodes
  val edge: Relation = Graphs.edge
  val path: Relation = Graphs.path

  def PE(graph: Graph): Program[FValue] = {
    val x = Variable("x", nodes)
    val y = Variable("y", nodes)
    val z = Variable("z", nodes)
    val ruleE = Rule(ve, path(x, y), edge(x, y))
    val ruleT = Rule(vt, path(x, z), path(x, y), edge(y, z))

    Program("PE", ruleE, ruleT)
  }

  def EP(graph: Graph): Program[FValue] = {
    val x = Variable("x", nodes)
    val y = Variable("y", nodes)
    val z = Variable("z", nodes)
    val ruleE = Rule(ve, path(x, y), edge(x, y))
    val ruleT = Rule(vt, path(x, z), edge(x, y), path(y, z))

    Program("EP", ruleE, ruleT)
  }

  def PP(graph: Graph): Program[FValue] = {
    val x = Variable("x", nodes)
    val y = Variable("y", nodes)
    val z = Variable("z", nodes)
    val ruleE = Rule(ve, path(x, y), edge(x, y))
    val ruleT = Rule(vt, path(x, z), path(x, y), path(y, z))

    Program("PP", ruleE, ruleT)
  }

}
