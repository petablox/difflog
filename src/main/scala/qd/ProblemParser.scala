package qd

import qd.Semiring.FValueSemiringObj

import scala.util.Random
import scala.util.matching.Regex
import scala.util.parsing.combinator._

class ProblemParser extends JavaTokenParsers {

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Main Block and Parser State

  def problem: Parser[State] = (// domainBlock | // Commented because domains are now implicitly declared
                                inputDeclBlock |
                                inventedDeclBlock |
                                outputDeclBlock |
                                edbBlock |
                                idbBlock |
                                ruleBlock).* ^^ { f =>
    f.foldLeft(initialState) { case (state, transformer) => transformer(state) }
  }

  case class State(inputRels: Set[Relation], inventedRels: Set[Relation], outputRels: Set[Relation],
                   edb: Set[(Relation, DTuple)], idb: Set[(Relation, DTuple)], rules: Set[Rule[FValue]]) {

    require(inputRels.intersect(inventedRels).isEmpty)
    require(inputRels.intersect(outputRels).isEmpty)
    require(inventedRels.intersect(outputRels).isEmpty)
    require(edb.intersect(idb).isEmpty)

    val allRels: Set[Relation] = inputRels ++ inventedRels ++ outputRels
    val allTuples: Set[(Relation, DTuple)] = edb ++ idb

    def addInputRel(rel: Relation): State = {
      require(!allRels.contains(rel), s"Relation $rel multiply declared")
      State(inputRels + rel, inventedRels, outputRels, edb, idb, rules)
    }

    def addInventedRel(rel: Relation): State = {
      require(!allRels.contains(rel), s"Relation $rel multiply declared")
      State(inputRels, inventedRels + rel, outputRels, edb, idb, rules)
    }

    def addOutputRel(rel: Relation): State = {
      require(!allRels.contains(rel), s"Relation $rel multiply declared")
      State(inputRels, inventedRels, outputRels + rel, edb, idb, rules)
    }

    def addEDBTuple(rt: (Relation, DTuple)): State = {
      require(!allTuples.contains(rt), s"Redeclaring tuple $rt")
      State(inputRels, inventedRels, outputRels, edb + rt, idb, rules)
    }

    def addIDBTuple(rt: (Relation, DTuple)): State = {
      require(!allTuples.contains(rt), s"Redeclaring tuple $rt")
      State(inputRels, inventedRels, outputRels, edb, idb + rt, rules)
    }

    def addRule(rule: Rule[FValue]): State = {
      require(rule.relations.forall(allRels))
      if (rules.contains(rule)) println(s"Warning: Redeclaring rule $rule")
      State(inputRels, inventedRels, outputRels, edb, idb, rules + rule)
    }

  }

  implicit val vs: FValueSemiring = FValueSemiringObj
  val initialState: State = State(Set(), Set(), Set(), Set(), Set(), Set())
  val rng: Random = Random
  var numTokens = 0
  def nextToken(): Token = { val ans = numTokens; numTokens = numTokens + 1; Token(s"R$ans") }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comments and identifiers
  // All valid Java identifiers may be used: See documentation for JavaTokenParsers.ident

  // Ignore C and C++-style comments. See: https://stackoverflow.com/a/5954831
  protected override val whiteSpace: Regex = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def identList: Parser[Seq[String]] = (ident ~ ("," ~ ident ^^ (_._2)).* ^^ mkList) | ("" ^^ (_ => Seq()))

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Domains and Relation Declarations

  // def domainBlock: Parser[Set[Domain]] = "Domain" ~ "{" ~ identList ~ "}" ^^ (_._1._2.map(Domain).toSet)

  def inputDeclBlock: Parser[State => State] = "Input" ~ "{" ~ relationList ~ "}" ^^ { f => state =>
    val newInputRelations = f._1._2
    newInputRelations.foldLeft(state)(_ addInputRel _)
  }

  def inventedDeclBlock: Parser[State => State] = "Invented" ~ "{" ~ relationList ~ "}" ^^ { f => state =>
    val newInventedRelations = f._1._2
    newInventedRelations.foldLeft(state)(_ addInventedRel _)
  }

  def outputDeclBlock: Parser[State => State] = "Output" ~ "{" ~ relationList ~ "}" ^^ { f => state =>
    val newOutputRelations = f._1._2
    newOutputRelations.foldLeft(state)(_ addOutputRel _)
  }

  def relationList: Parser[Seq[Relation]] = (relationDecl ~ ("," ~ relationDecl ^^ (_._2)).* ^^ mkList) |
                                            ("" ^^ (_ => Seq()))

  def relationDecl: Parser[Relation] = ident ~ "(" ~ identList ~ ")" ^^ { f =>
    val relName = f._1._1._1
    val signature = f._1._2.map(Domain)
    Relation(relName, signature:_*)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // EDB and IDB Declarations

  def edbBlock: Parser[State => State] = "EDB" ~ "{" ~ tupleList ~ "}" ^^ { f => state =>
    val newEDBTuples = f._1._2.map(_(state))
    newEDBTuples.foldLeft(state)(_ addEDBTuple _)
  }

  def idbBlock: Parser[State => State] = "IDB" ~ "{" ~ tupleList ~ "}" ^^ { f => state =>
    val newIDBTuples = f._1._2.map(_(state))
    newIDBTuples.foldLeft(state)(_ addIDBTuple _)
  }

  def tupleList: Parser[Seq[State => (Relation, DTuple)]] = (tupleDecl ~ ("," ~ tupleDecl ^^ (_._2)).* ^^ mkList) |
                                                            ("" ^^ (_ => Seq()))

  def tupleDecl: Parser[State => (Relation, DTuple)] = ident ~ "(" ~ identList ~ ")" ^^ { f => state =>
    val relName = f._1._1._1
    val fieldNames = f._1._2
    val t = s"$relName(${fieldNames.mkString(", ")})"

    val relOpt = state.allRels.find(_.name == relName)
    require(relOpt.nonEmpty, s"Unable to resolve relation named $relName")
    val relation = relOpt.get

    require(relation.arity == fieldNames.size, s"Arity mismatch between tuple $t and relation $relation")
    val fields = fieldNames.zip(relation.signature).map { case (c, d) => Constant(c, d) }
    (relation, DTuple(fields:_*))
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Rule Declaration Block

  def ruleBlock: Parser[State => State] = augmentationRuleBlock | explicitRuleBlock

  // One may either write:
  // 1. AllRules(2, 3), to indicate all rules with 2 literals and 3 variables and randomly initialized coefficients, or
  // 2. AllRules(2, 3, 0.6), to indicate all rules with 2 literals and 3 variables and coefficients set to 0.6.

  def augmentationRuleBlock: Parser[State => State] = {
    "AllRules" ~ "(" ~
                        (wholeNumber ^^ (_.toInt)) ~ "," ~ (wholeNumber ^^ (_.toInt)) ~
                        ("," ~ decimalNumber ^^ (f => Some(f._2.toDouble))| "" ^^ (_ => None)) ~
                 ")" ^^ {
      f => state =>
        val maxLiterals = f._1._1._1._1._2
        require(maxLiterals > 0, s"Expected strictly positive value for maxLiterals; instead found $maxLiterals")
        val maxVars = f._1._1._2
        require(maxVars > 0, s"Expected strictly positive value for maxVars; instead found $maxVars")

        val weightSpec = f._1._2
        def weight(l: Literal, ls: Set[Literal]): FValue = if (weightSpec.nonEmpty) FValue(weightSpec.get, nextToken())
                                                           else FValue(rng.nextDouble(), nextToken())

        val p = Program.skeleton[FValue]("Program", state.inputRels, state.inventedRels, state.outputRels,
                                         weight, maxLiterals, maxVars)
        p.rules.foldLeft(state)(_ addRule _)
    }
  }

  def explicitRuleBlock: Parser[State => State] = "Rules" ~ "{" ~ rep(ruleDecl) ~ "}" ^^ { f => state =>
    val newRules = f._1._2.map(_(state))
    newRules.foldLeft(state)(_ addRule _)
  }

  // The syntax of rules is as usual, terminated with a period.
  // The rule weight may either be left unspecified, in which case it is initialized uniformly at random, or
  // be concretely specified with a colon-prefix. For example:
  // path(a, c) :- edge(a, b), path(b, c).
  // 0.7: path(a, c) :- path(c, a).

  def ruleDecl: Parser[State => Rule[FValue]] = {
    (decimalNumber ~ ":" ^^ (f => f._1.toDouble)| "" ^^ (_ => rng.nextDouble())) ~
    literal ~ ":-" ~ literalSeq ~ "." ^^ { f => state =>
      val coeff = FValue(f._1._1._1._1, nextToken())
      val head = f._1._1._1._2(state)
      val body = f._1._2.map(_(state))

      val ans = Rule(coeff, head, body.toSet)
      val allVars = ans.variables.groupBy(_.name)
      for ((name, instances) <- allVars) {
        require(instances.size == 1, s"Multiple incompatible uses of variable name $name")
      }
      ans
    }
  }

  def literalSeq: Parser[Seq[State => Literal]] = literal ~ ("," ~ literal ^^ (_._2)).* ^^ mkList |
                                                  "" ^^ (_ => Seq())

  def literal: Parser[State => Literal] = ident ~ "(" ~ identList ~ ")" ^^ { f => state =>
    val relName = f._1._1._1
    val fieldNames = f._1._2
    val litString = s"$relName(${fieldNames.mkString(", ")})"

    val optRel = state.allRels.find(_.name == relName)
    require(optRel.nonEmpty, s"Unable to resolve relation $relName")
    val rel = optRel.get
    require(rel.arity == fieldNames.size, s"Arity mismatch between relation $rel and literal $litString")

    val fields = fieldNames.zip(rel.signature).map { case (name, domain) => Variable(name, domain) }

    Literal(rel, fields:_*)
  }

}
