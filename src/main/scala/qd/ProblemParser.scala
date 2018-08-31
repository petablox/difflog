package qd

import scala.util.parsing.combinator._

class ProblemParser extends JavaTokenParsers {
  def identList: Parser[Seq[String]] = (ident ~ ("," ~ ident ^^ (_._2)).* ^^ mkList) | ("" ^^ (_ => Seq()))

  // Commented because domain declarations are now implicit
  // def domainBlock: Parser[Set[Domain]] = "Domain" ~ "{" ~ identList ~ "}" ^^ (_._1._2.map(Domain).toSet)

  def relationDecl: Parser[Relation] = ident ~ "(" ~ identList ~ ")" ^^ { f =>
    val relName = f._1._1._1
    val signature = f._1._2.map(Domain)
    Relation(relName, signature:_*)
  }
  def relationList: Parser[Seq[Relation]] = (relationDecl ~ ("," ~ relationDecl ^^ (_._2)).* ^^ mkList) |
                                            ("" ^^ (_ => Seq()))
  def relationSet: Parser[Set[Relation]] = relationList ^^ { rs =>
    for (r <- rs) { require(rs.count(_.name == r.name) == 1, s"Relation ${r.name} multiply declared") }
    rs.toSet
  }

  def inputDeclBlock: Parser[Set[Relation]] = "Input" ~ "{" ~ relationSet ~ "}" ^^ (_._1._2)
  def inventedDeclBlock: Parser[Set[Relation]] = "Invented" ~ "{" ~ relationSet ~ "}" ^^ (_._1._2)
  def outputDeclBlock: Parser[Set[Relation]] = "Output" ~ "{" ~ relationSet ~ "}" ^^ (_._1._2)

  def tupleDecl: Parser[Set[Relation] => DTuple] = ident ~ "(" ~ identList ~ ")" ^^ { f => allRels =>
    val relName = f._1._1._1
    val fieldNames = f._1._2
    val t = s"$relName(${fieldNames.mkString(", ")})"

    val relOpt = allRels.find(_.name == relName)
    require(relOpt.nonEmpty, s"Unable to resolve relation named $relName")
    val relation = relOpt.get

    require(relation.arity == fieldNames.size, s"Arity mismatch between tuple $t and relation $relation")
    val fields = fieldNames.zip(relation.signature).map { case (c, d) => Constant(c, d) }
    DTuple(fields:_*)
  }
  def tupleList: Parser[Seq[Set[Relation] => DTuple]] = (tupleDecl ~ ("," ~ tupleDecl ^^ (_._2)).* ^^ mkList) |
                                                        ("" ^^ (_ => Seq()))
  def tupleSet: Parser[Set[Relation] => Set[DTuple]] = tupleList ^^ { fs => allRels =>
    val allTuples = fs.map(_(allRels))
    for (t <- allTuples) { require(allTuples.count(_ == t) == 1, s"Tuple $t multiply declared") }
    allTuples.toSet
  }

  case class State(inputRels: Set[Relation], inventedRels: Set[Relation], outputRels: Set[Relation],
                   edb: Set[DTuple], idb: Set[DTuple])

  def edbBlock: Parser[Any] = "EDB" ~ "{" ~ tupleList ~ "}"
  def idbBlock: Parser[Any] = "IDB" ~ "{" ~ tupleList ~ "}"

  def problem: Parser[Any] = (// domainBlock |
                              inputDeclBlock |
                              inventedDeclBlock |
                              outputDeclBlock |
                              edbBlock |
                              idbBlock).*
}
