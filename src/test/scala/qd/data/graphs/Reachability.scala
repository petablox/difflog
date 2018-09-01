package qd
package data
package graphs

object Reachability {

  val ve: FValue = FValue(0.8, Token("E"))
  val vt: FValue = FValue(0.9, Token("T"))

  val node: Domain = Graphs.node
  val edge: Relation = Graphs.edge
  val path: Relation = Graphs.path

  val x = Variable("x", node)
  val y = Variable("y", node)
  val z = Variable("z", node)

  val ruleE = Rule(ve, path(x, y), edge(x, y))
  val rulePE = Rule(vt, path(x, z), path(x, y), edge(y, z))
  val ruleEP = Rule(vt, path(x, z), edge(x, y), path(y, z))
  val rulePP = Rule(vt, path(x, z), path(x, y), path(y, z))

  val PE: Program[FValue] = Program("PE", ruleE, rulePE)
  val EP: Program[FValue] = Program("EP", ruleE, ruleEP)
  val PP: Program[FValue] = Program("PP", ruleE, rulePP)

  val Programs: Set[Program[FValue]] = Set(PE, EP, PP)

}
