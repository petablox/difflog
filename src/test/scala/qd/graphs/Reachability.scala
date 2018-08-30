package qd
package graphs

object Reachability {

  val ve: FValue = FValue(0.8, Token("E"))
  val vt: FValue = FValue(0.9, Token("T"))

  val nodes: Domain = Graphs.nodes
  val edge: Relation = Graphs.edge
  val path: Relation = Graphs.path

  val x = Variable("x", nodes)
  val y = Variable("y", nodes)
  val z = Variable("z", nodes)

  val ruleE = Rule(ve, path(x, y), edge(x, y))
  val rulePE = Rule(vt, path(x, z), path(x, y), edge(y, z))
  val ruleEP = Rule(vt, path(x, z), edge(x, y), path(y, z))
  val rulePP = Rule(vt, path(x, z), path(x, y), path(y, z))

  val PE: Program[FValue] = Program("PE", ruleE, rulePE)
  val EP: Program[FValue] = Program("EP", ruleE, ruleEP)
  val PP: Program[FValue] = Program("PP", ruleE, rulePP)

  val Programs: Set[Program[FValue]] = Set(PE, EP, PP)

}
