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
class Valuation private (map: Map[Variable, Atom], val score: Value) extends Map[Variable, Atom] {
  override def get(variable: Variable): Option[Atom] = map.get(variable)
  override def iterator: Iterator[(Variable, Atom)] = map.iterator

  def +(va: (Variable, Atom)): Valuation = {
    val (variable, atom) = va
    require(variable.domain.contains(atom))
    new Valuation(map + va, score)
  }
  override def +[V >: Atom](kv: (Variable, V)): Map[Variable, V] = map + kv
  override def -(variable: Variable): Valuation = Valuation(map - variable, score)
  def *(coeff: Value): Valuation = Valuation(map, score * coeff)

  def filter(literal: Literal): Seq[Option[Atom]] = literal.parameters.map {
    case v @ Variable(_, _) => map.get(v)
    case Constant(atom, _) => Some(atom)
  }
}

object Valuation {
  def apply(map: Map[Variable, Atom], score: Value): Valuation = {
    require(map.forall { case (variable, atom) => variable.domain.contains(atom) })
    new Valuation(map, score)
  }
  def apply(): Valuation = new Valuation(Map(), One)
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Literal and rules

case class Literal(relation: Relation, parameters: Parameter*) {
  require(relation.signature == parameters.map(_.domain))
  val freeVariables: Set[Variable] = parameters.collect({ case v: Variable => v }).toSet
  override def toString: String = s"${relation.name}(${parameters.mkString(", ")})"

  def concretize(valuation: Valuation): Map[DTuple, Value] = {
    var completeValuations = Set(valuation)
    for (v <- parameters.collect { case v: Variable => v }) {
      completeValuations = completeValuations.flatMap(valPrime => {
        if (!valPrime.contains(v)) v.domain.map(atom => valPrime + (v -> atom))
        else Set(valPrime)
      })
    }

    (for (valPrime <- completeValuations) yield {
      val fields = parameters.map {
        case v @ Variable(_, _) => valPrime(v)
        case Constant(c, _) => c
      }
      DTuple(fields:_*) -> valPrime.score
    }).toMap
  }
}

case class Rule(name: Any, coeff: Value, head: Literal, body: Set[Literal]) {
  val freeVariables: Set[Variable] = (body + head).flatMap(_.freeVariables)
  override def toString: String = s"$name: $head :- ${body.mkString(", ")}."
}

object Rule {
  def apply(name: Any, coeff: Value, head: Literal,
            firstBodyLiteral: Literal, remainingBodyLiteral: Literal*): Rule = {
    Rule(name, coeff, head, (firstBodyLiteral +: remainingBodyLiteral).toSet)
  }
  def apply(name: Any, coeff: Value, head: Literal): Rule = Rule(name, coeff, head, Set[Literal]())
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
