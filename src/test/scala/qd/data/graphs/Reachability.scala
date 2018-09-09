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

  val ruleE = Rule(ve, path(List(x, y)), List(edge(List(x, y))))
  val rulePE = Rule(vt, path(List(x, z)), List(path(List(x, y)), edge(List(y, z))))
  val ruleEP = Rule(vt, path(List(x, z)), List(edge(List(x, y)), path(List(y, z))))
  val rulePP = Rule(vt, path(List(x, z)), List(path(List(x, y)), path(List(y, z))))

  val PE: (String, Set[Rule[FValue]]) = ("PE", Set(ruleE, rulePE))
  val EP: (String, Set[Rule[FValue]]) = ("EP", Set(ruleE, ruleEP))
  val PP: (String, Set[Rule[FValue]]) = ("PP", Set(ruleE, rulePP))

  val Programs: Set[(String, Set[Rule[FValue]])] = Set(PE, EP, PP)

}
