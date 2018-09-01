package qd

import scala.util.matching.Regex
import scala.util.parsing.combinator._

class ProblemParser extends JavaTokenParsers {

  case class State(inputRels: Set[Relation], inventedRels: Set[Relation], outputRels: Set[Relation],
                   edb: Set[(Relation, DTuple)], idb: Set[(Relation, DTuple)]) {

    require(inputRels.intersect(inventedRels).isEmpty)
    require(inputRels.intersect(outputRels).isEmpty)
    require(inventedRels.intersect(outputRels).isEmpty)
    require(edb.intersect(idb).isEmpty)

    val allRels: Set[Relation] = inputRels ++ inventedRels ++ outputRels
    val allTuples: Set[(Relation, DTuple)] = edb ++ idb

    def addInputRel(rel: Relation): State = {
      require(!allRels.contains(rel))
      State(inputRels + rel, inventedRels, outputRels, edb, idb)
    }

    def addInventedRel(rel: Relation): State = {
      require(!allRels.contains(rel))
      State(inputRels, inventedRels + rel, outputRels, edb, idb)
    }

    def addOutputRel(rel: Relation): State = {
      require(!allRels.contains(rel))
      State(inputRels, inventedRels, outputRels + rel, edb, idb)
    }

    def addEDBTuple(rt: (Relation, DTuple)): State = {
      require(!allTuples.contains(rt))
      State(inputRels, inventedRels, outputRels, edb + rt, idb)
    }

    def addIDBTuple(rt: (Relation, DTuple)): State = {
      require(!allTuples.contains(rt))
      State(inputRels, inventedRels, outputRels, edb, idb + rt)
    }

  }

  val initialState: State = State(Set(), Set(), Set(), Set(), Set())

  // Ignore C and C++-style comments. See: https://stackoverflow.com/a/5954831
  protected override val whiteSpace: Regex = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

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

  def inputDeclBlock: Parser[State => State] = "Input" ~ "{" ~ relationSet ~ "}" ^^ { f => state =>
    val newInputRelations = f._1._2
    newInputRelations.foldLeft(state)(_ addInputRel _)
  }
  def inventedDeclBlock: Parser[State => State] = "Invented" ~ "{" ~ relationSet ~ "}" ^^ { f => state =>
    val newInventedRelations = f._1._2
    newInventedRelations.foldLeft(state)(_ addInventedRel _)
  }
  def outputDeclBlock: Parser[State => State] = "Output" ~ "{" ~ relationSet ~ "}" ^^ { f => state =>
    val newOutputRelations = f._1._2
    newOutputRelations.foldLeft(state)(_ addOutputRel _)
  }

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
  def tupleList: Parser[Seq[State => (Relation, DTuple)]] = (tupleDecl ~ ("," ~ tupleDecl ^^ (_._2)).* ^^ mkList) |
                                                            ("" ^^ (_ => Seq()))
  def tupleSet: Parser[State => Set[(Relation, DTuple)]] = tupleList ^^ { fs => state =>
    val allTuples = fs.map(_(state))
    for (t <- allTuples) {
      require(allTuples.count(_ == t) == 1, s"Tuple $t multiply declared. allTuples: $allTuples")
      require(!state.edb.contains(t), s"Tuple $t multiply declared. allTuples: $allTuples")
      require(!state.idb.contains(t), s"Tuple $t multiply declared. allTuples: $allTuples")
    }
    allTuples.toSet
  }

  def edbBlock: Parser[State => State] = "EDB" ~ "{" ~ tupleSet ~ "}" ^^ { f => state =>
    val newEDBTuples = f._1._2(state)
    newEDBTuples.foldLeft(state)(_ addEDBTuple _)
  }
  def idbBlock: Parser[State => State] = "IDB" ~ "{" ~ tupleSet ~ "}" ^^ { f => state =>
    val newIDBTuples = f._1._2(state)
    newIDBTuples.foldLeft(state)(_ addIDBTuple _)
  }

  def problem: Parser[State] = (// domainBlock |
                                inputDeclBlock |
                                inventedDeclBlock |
                                outputDeclBlock |
                                edbBlock |
                                idbBlock).* ^^ { f =>
    f.foldLeft(initialState) { case (state, transformer) => transformer(state) }
  }
}
