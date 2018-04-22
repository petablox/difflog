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
  def apply(): Valuation = new Valuation(Map())
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Literal and rules

case class Literal(relation: Relation, parameters: Parameter*) {
  require(relation.signature == parameters.map(_.domain))
  val freeVariables: Set[Variable] = parameters.collect({ case v: Variable => v }).toSet
  override def toString: String = s"${relation.name}(${parameters.mkString(", ")})"

  def concretize(valuation: Valuation): Set[DTuple] = {
    var completeValuations = Set(valuation)
    for (v <- parameters.collect { case v: Variable => v }) {
      completeValuations = completeValuations.flatMap(valPrime => {
        if (!valPrime.contains(v)) v.domain.map(atom => valPrime + (v -> atom))
        else Set(valPrime)
      })
    }

    for (valPrime <- completeValuations) yield {
      val fields = parameters.map {
        case v @ Variable(_, _) => valPrime(v)
        case Constant(c, _) => c
      }
      DTuple(fields:_*)
    }
  }
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

case class Program(name: Any, rules: Set[Rule]) {
  val allRelations: Set[Relation] = rules.flatMap(rule => rule.body.map(_.relation) + rule.head.relation)
  val allDomains: Set[Domain] = allRelations.flatMap(_.signature)
  override def toString: String = rules.mkString(System.lineSeparator())
}

object Program {
  def apply(name: Any, firstRule: Rule, remainingRules: Rule*): Program = {
    Program(name, (firstRule +: remainingRules).toSet)
  }
  def apply(name: Any): Program = Program(name, Set[Rule]())
}

abstract class Evaluator(val name: Any, val program: Program) extends (Config => Config) {
  val rules: Set[Rule] = program.rules
  val allRelations: Set[Relation] = program.allRelations
  val allDomains: Set[Domain] = program.allDomains
}
