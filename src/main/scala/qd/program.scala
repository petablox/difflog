package qd

import scala.collection.mutable
import scala.util.hashing.MurmurHash3

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Parameters: Variables or Constants

sealed abstract class Parameter(val domain: Domain)
case class Variable(name: Any, override val domain: Domain) extends Parameter(domain) {
  override val hashCode: Int = MurmurHash3.finalizeHash(MurmurHash3.mix(name.##, domain.hashCode), 2)
  override def toString: String = name.toString
}
case class Constant(value: Atom, override val domain: Domain) extends Parameter(domain) {
  require(domain.contains(value))
  override def toString: String = value.toString
}

// A valuation is a mapping from variables to atoms
// They are intermediate objects encountered while applying rules
class Valuation[T <: Value[T]] private (val backingMap: Map[Variable, Atom], val score: T) extends Map[Variable, Atom] {
  override def get(variable: Variable): Option[Atom] = backingMap.get(variable)
  override def iterator: Iterator[(Variable, Atom)] = backingMap.iterator

  def +(va: (Variable, Atom)): Valuation[T] = {
    val (variable, atom) = va
    require(variable.domain.contains(atom))
    new Valuation(backingMap + va, score)
  }
  override def +[V >: Atom](kv: (Variable, V)): Map[Variable, V] = backingMap + kv
  override def -(variable: Variable): Valuation[T] = Valuation(backingMap - variable, score)
  def *(coeff: T): Valuation[T] = Valuation(backingMap, score * coeff)

  def toFilter(literal: Literal): Seq[Option[Atom]] = literal.parameters.map {
    case v @ Variable(_, _) => backingMap.get(v)
    case Constant(atom, _) => Some(atom)
  }

  def project(rvs: Set[Variable]): Valuation[T] = new Valuation(backingMap.filterKeys(v => rvs.contains(v)), score)
}

object Valuation {
  def apply[T <: Value[T]](map: Map[Variable, Atom], score: T): Valuation[T] = {
    require(map.forall { case (variable, atom) => variable.domain.contains(atom) })
    new Valuation(map, score)
  }
  def Empty[T <: Value[T]](implicit num : OneAndZero[T]): Valuation[T] = new Valuation(Map(), num.One)
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Literal and rules

case class Literal(relation: Relation, parameters: Parameter*) {
  require(relation.signature == parameters.map(_.domain))
  val freeVariables: Set[Variable] = parameters.collect({ case v: Variable => v }).toSet
  override def toString: String = s"${relation.name}(${parameters.mkString(", ")})"

  def concretize[T <: Value[T]](valuation: Valuation[T]): Map[DTuple, Value[T]] = {
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

case class Rule[T <: Value[T]](name: Any, coeff: T, head: Literal, body: Set[Literal]) {
  val freeVariables: Set[Variable] = (body + head).flatMap(_.freeVariables)
  override def toString: String = s"$name: $head :- ${body.mkString(", ")}."
}

object Rule {
  def apply[T <: Value[T]](name: Any, coeff: T, head: Literal,
            firstBodyLiteral: Literal, remainingBodyLiteral: Literal*): Rule[T] = {
    Rule(name, coeff, head, (firstBodyLiteral +: remainingBodyLiteral).toSet)
  }
  def apply[T <: Value[T]](name: Any, coeff: T, head: Literal): Rule[T] = Rule(name, coeff, head, Set[Literal]())
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Programs and evaluators

case class Program[T <: Value[T]](name: Any, rules: Set[Rule[T]]) {
  val allRelations: Set[Relation] = rules.flatMap(rule => rule.body.map(_.relation) + rule.head.relation)
  val allDomains: Set[Domain] = allRelations.flatMap(_.signature)
  override def toString: String = rules.mkString(System.lineSeparator())
}

object Program {
  def apply[T <: Value[T]](name: Any, firstRule: Rule[T], remainingRules: Rule[T]*): Program[T] = {
    Program(name, (firstRule +: remainingRules).toSet)
  }
  def apply[T <: Value[T]](name: Any): Program[T] = Program(name, Set[Rule[T]]())
}

abstract class Evaluator[T <: Value[T]](val name: Any, val program: Program[T]) extends (Config[T] => Config[T]) {
  val rules: Set[Rule[T]] = program.rules
  val allRelations: Set[Relation] = program.allRelations
  val allDomains: Set[Domain] = program.allDomains
  protected val time: mutable.Map[Rule[T], Long] = new mutable.HashMap()
  def getTime: Map[Rule[T], Long] = time.toMap
}
