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

  val ruleE = Rule(ve, path(Vector(x, y)), Vector(edge(Vector(x, y))))
  val rulePE = Rule(vt, path(Vector(x, z)), Vector(path(Vector(x, y)), edge(Vector(y, z))))
  val ruleEP = Rule(vt, path(Vector(x, z)), Vector(edge(Vector(x, y)), path(Vector(y, z))))
  val rulePP = Rule(vt, path(Vector(x, z)), Vector(path(Vector(x, y)), path(Vector(y, z))))

  val PE: (String, Set[Rule[FValue]]) = ("PE", Set(ruleE, rulePE))
  val EP: (String, Set[Rule[FValue]]) = ("EP", Set(ruleE, ruleEP))
  val PP: (String, Set[Rule[FValue]]) = ("PP", Set(ruleE, rulePP))

  val Programs: Set[(String, Set[Rule[FValue]])] = Set(PE, EP, PP)

}
