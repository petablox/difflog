package qd

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Parameters: Variables or Constants

sealed abstract class Parameter(val domain: Domain)
case class Variable(name: Any, override val domain: Domain) extends Parameter(domain) {
  override def toString: String = name.toString
}
case class Constant(value: Atom, override val domain: Domain) extends Parameter(domain) {
  require(domain.contains(value))
  override def toString: String = value.toString
}

// A valuation is a mapping from variables to atoms
// They are intermediate objects encountered while applying rules
class Valuation private (map: Map[Variable, Atom]) {
  def apply(variable: Variable): Atom = map(variable)
  def contains(variable: Variable): Boolean = map.contains(variable)
  def +(va: (Variable, Atom)): Valuation = {
    val (variable, atom) = va
    require(variable.domain.contains(atom))
    new Valuation(map + va)
  }
}

object Valuation {
  def apply(map: Map[Variable, Atom]): Valuation = {
    require(map.forall { case (variable, atom) => variable.domain.contains(atom) })
    new Valuation(map)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Literal and rules

case class Literal(relation: Relation, parameters: Parameter*) {
  require(relation.numFields == parameters.length)
  require(relation.signature.zip(parameters).forall(dp => dp._1 == dp._2.domain))
  val freeVariables: Set[Variable] = parameters.filter(_.isInstanceOf[Variable])
                                               .map(_.asInstanceOf[Variable])
                                               .toSet
  override def toString: String = s"${relation.name}(${parameters.mkString(", ")})"
}

case class Rule(name: Any, head: Literal, body: Set[Literal]) {
  val freeVariables: Set[Variable] = (body + head).flatMap(_.freeVariables)
  override def toString: String = s"$name: $head :- ${body.mkString(", ")}."
}

object Rule {
  def apply(name: Any, head: Literal, firstBodyLiteral: Literal, remainingBodyLiteral: Literal*): Rule = {
    Rule(name, head, (firstBodyLiteral +: remainingBodyLiteral).toSet)
  }
  def apply(name: Any, head: Literal): Rule = Rule(name, head, Set[Literal]())
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Weighted literals and weighted rules

case class WeightedLiteral(coefficient: Double, relation: Relation, parameters: Parameter*) {
  require(0.0 <= coefficient && coefficient <= 1.0)
  require(relation.signature == parameters.map(_.domain))
  override def toString: String = f"$coefficient%.2f ${relation.name}%s(${parameters.mkString(", ")}%s)"
}

case class WeightedRule(name: Any, coefficient: Double, head: Literal, body: Literal*) {
  require(0.0 <= coefficient && coefficient <= 1.0)
  override def toString: String = f"$name%s ($coefficient%.2f): $head%s :- ${body.mkString(", ")}%s."
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Programs and evaluators

case class Program(rules: Set[Rule]) {
  val allSchemas: Set[Relation] = rules.flatMap(rule => rule.body.map(_.relation) + rule.head.relation)
  val allDomains: Set[Domain] = allSchemas.flatMap(_.signature)
  override def toString: String = rules.mkString(System.lineSeparator())
}

object Program {
  def apply(firstRule: Rule, remainingRules: Rule*): Program = Program((firstRule +: remainingRules).toSet)
  def apply(): Program = Program(Set[Rule]())
}

abstract class Evaluator(program: Program) {
  def eval(edb: Config): Config

  val rules: Set[Rule] = program.rules
  val allSchemas: Set[Relation] = program.allSchemas
  val allDomains: Set[Domain] = program.allDomains
}
