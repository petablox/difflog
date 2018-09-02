package qd

import qd.Semiring.FValueSemiringObj

import scala.util.Random
import scala.util.matching.Regex
import scala.util.parsing.combinator._

class ProblemParser extends JavaTokenParsers {

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Main Block

  def problem: Parser[Problem] = (// domainBlock | // Commented because domains are now implicitly declared
                                  inputDeclBlock |
                                  inventedDeclBlock |
                                  outputDeclBlock |
                                  edbBlock |
                                  idbBlock |
                                  ruleBlock).* ^^ { f =>
    f.foldLeft(initialProblem) { case (problem, transformer) => transformer(problem) }
  }

  implicit val vs: FValueSemiring = FValueSemiringObj
  val initialProblem: Problem = Problem()
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

  def inputDeclBlock: Parser[Problem => Problem] = "Input" ~ "{" ~ relationList ~ "}" ^^ { f => problem =>
    val newInputRelations = f._1._2
    newInputRelations.foldLeft(problem)(_ addInputRel _)
  }

  def inventedDeclBlock: Parser[Problem => Problem] = "Invented" ~ "{" ~ relationList ~ "}" ^^ { f => problem =>
    val newInventedRelations = f._1._2
    newInventedRelations.foldLeft(problem)(_ addInventedRel _)
  }

  def outputDeclBlock: Parser[Problem => Problem] = "Output" ~ "{" ~ relationList ~ "}" ^^ { f => problem =>
    val newOutputRelations = f._1._2
    newOutputRelations.foldLeft(problem)(_ addOutputRel _)
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

  def edbBlock: Parser[Problem => Problem] = "EDB" ~ "{" ~ tupleList ~ "}" ^^ { f => problem =>
    val newEDBTuples = f._1._2.map(_(problem))
    newEDBTuples.foldLeft(problem)(_ addEDBTuple _)
  }

  def idbBlock: Parser[Problem => Problem] = "IDB" ~ "{" ~ tupleList ~ "}" ^^ { f => problem =>
    val newIDBTuples = f._1._2.map(_(problem))
    newIDBTuples.foldLeft(problem)(_ addIDBTuple _)
  }

  def tupleList: Parser[Seq[Problem => (Relation, DTuple)]] = (tupleDecl ~ ("," ~ tupleDecl ^^ (_._2)).* ^^ mkList) |
                                                            ("" ^^ (_ => Seq()))

  def tupleDecl: Parser[Problem => (Relation, DTuple)] = ident ~ "(" ~ identList ~ ")" ^^ { f => problem =>
    val relName = f._1._1._1
    val fieldNames = f._1._2
    val t = s"$relName(${fieldNames.mkString(", ")})"

    val relOpt = problem.allRels.find(_.name == relName)
    require(relOpt.nonEmpty, s"Unable to resolve relation named $relName")
    val relation = relOpt.get

    require(relation.arity == fieldNames.size, s"Arity mismatch between tuple $t and relation $relation")
    val fields = fieldNames.zip(relation.signature).map { case (c, d) => Constant(c, d) }
    (relation, DTuple(fields:_*))
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Rule Declaration Block

  def ruleBlock: Parser[Problem => Problem] = augmentationRuleBlock | explicitRuleBlock

  // One may either write:
  // 1. AllRules(2, 3), to indicate all rules with 2 literals and 3 variables and randomly initialized coefficients, or
  // 2. AllRules(2, 3, 0.6), to indicate all rules with 2 literals and 3 variables and coefficients set to 0.6.

  def augmentationRuleBlock: Parser[Problem => Problem] = {
    "AllRules" ~ "(" ~
                        (wholeNumber ^^ (_.toInt)) ~ "," ~ (wholeNumber ^^ (_.toInt)) ~
                        ("," ~ decimalNumber ^^ (f => Some(f._2.toDouble))| "" ^^ (_ => None)) ~
                 ")" ^^ {
      f => p0 =>
        val maxLiterals = f._1._1._1._1._2
        require(maxLiterals > 0, s"Expected strictly positive value for maxLiterals; instead found $maxLiterals")
        val maxVars = f._1._1._2
        require(maxVars > 0, s"Expected strictly positive value for maxVars; instead found $maxVars")

        val weightSpec = f._1._2
        def weight(l: Literal, ls: Set[Literal]): (Token, FValue) = {
          val token = nextToken()
          val value = if (weightSpec.nonEmpty) FValue(weightSpec.get, token) else FValue(rng.nextDouble(), token)
          (token, value)
        }

        val skeleton = Program.skeleton[FValue](p0.inputRels, p0.inventedRels, p0.outputRels,
                                                weight, maxLiterals, maxVars)
        val pos = TokenVec(skeleton._1.mapValues(_.v))
        val rules = skeleton._2

        val p1 = pos.foldLeft(p0) { case (p, (token, value)) => p.addToken(token, value) }
        val p2 = rules.foldLeft(p1) { case (p, rule) => p.addRule(rule.coeff.l, rule.head, rule.body) }
        p2
    }
  }

  def explicitRuleBlock: Parser[Problem => Problem] = "Rules" ~ "{" ~ rep(ruleDecl) ~ "}" ^^ { f =>p0 =>
    val newRules = f._1._2.map(_(p0))
    val p1 = newRules.foldLeft(p0){ case (p, (token, value, _, _)) => p.addToken(token, value) }
    val p2 = newRules.foldLeft(p1){ case (p, (token, _, head, body)) => p.addRule(token, head, body) }
    p2
  }

  // The syntax of rules is as usual, terminated with a period.
  // The rule weight may either be left unspecified, in which case it is initialized uniformly at random, or
  // be concretely specified with a colon-prefix. For example:
  // path(a, c) :- edge(a, b), path(b, c).
  // 0.7: path(a, c) :- path(c, a).

  def ruleDecl: Parser[Problem => (Token, Double, Literal, Set[Literal])] = {
    (decimalNumber ~ ":" ^^ (f => f._1.toDouble)| "" ^^ (_ => rng.nextDouble())) ~
    literal ~ ":-" ~ literalSeq ~ "." ^^ { f => problem =>
      val token = nextToken()
      val value = f._1._1._1._1
      val head = f._1._1._1._2(problem)
      val body = f._1._2.map(_(problem)).toSet

      val allVars = body.flatMap(_.variables) ++ head.variables
      for ((name, instances) <- allVars.groupBy(_.name)) {
        require(instances.size == 1, s"Multiple incompatible uses of variable name $name")
      }

      (token, value, head, body)
    }
  }

  def literalSeq: Parser[Seq[Problem => Literal]] = literal ~ ("," ~ literal ^^ (_._2)).* ^^ mkList |
                                                  "" ^^ (_ => Seq())

  def literal: Parser[Problem => Literal] = ident ~ "(" ~ identList ~ ")" ^^ { f => problem =>
    val relName = f._1._1._1
    val fieldNames = f._1._2
    val litString = s"$relName(${fieldNames.mkString(", ")})"

    val optRel = problem.allRels.find(_.name == relName)
    require(optRel.nonEmpty, s"Unable to resolve relation $relName")
    val rel = optRel.get
    require(rel.arity == fieldNames.size, s"Arity mismatch between relation $rel and literal $litString")

    val fields = fieldNames.zip(rel.signature).map { case (name, domain) => Variable(name, domain) }

    Literal(rel, fields:_*)
  }

}
