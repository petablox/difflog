package qd.problem

import qd._
import qd.instance.Config
import qd.util.Contract

class Problem private (
                        val inputRels: Set[Relation],
                        val inventedRels: Set[Relation],
                        val outputRels: Set[Relation],

                        val edb: Config[FValue],
                        val idb: Config[FValue],

                        val pos: TokenVec,
                        val rules: Set[Rule]
                      ) {

  def allTokens: Set[Token] = pos.keySet
  def findRel(pred: Relation => Boolean): Option[Relation] = inputRels.find(pred)
                                                                      .orElse(inventedRels.find(pred))
                                                                      .orElse(outputRels.find(pred))
  def knownRelation(rel: Relation): Boolean = inputRels.contains(rel) ||
                                              inventedRels.contains(rel) ||
                                              outputRels.contains(rel)
  def isNewRelation(rel: Relation): Boolean = !knownRelation(rel)

  def addInputRel(rel: Relation): Problem = {
    Contract.require(isNewRelation(rel), s"Relation $rel multiply declared")
    new Problem(inputRels + rel, inventedRels, outputRels, edb, idb, pos, rules)
  }

  def addInputRels(rels: Relation*): Problem = {
    val newInputRels = rels.foldLeft(inputRels) { case (nirs, rel) =>
      Contract.require(isNewRelation(rel), s"Relation $rel multiply declared")
      nirs + rel
    }
    new Problem(newInputRels, inventedRels, outputRels, edb, idb, pos, rules)
  }

  def addInventedRel(rel: Relation): Problem = {
    Contract.require(isNewRelation(rel), s"Relation $rel multiply declared")
    new Problem(inputRels, inventedRels + rel, outputRels, edb, idb, pos, rules)
  }

  def addInventedRels(rels: Relation*): Problem = {
    val newInventedRels = rels.foldLeft(inventedRels) { case (nirs, rel) =>
      Contract.require(isNewRelation(rel), s"Relation $rel multiply declared")
      nirs + rel
    }
    new Problem(inputRels, newInventedRels, outputRels, edb, idb, pos, rules)
  }

  def addOutputRel(rel: Relation): Problem = {
    Contract.require(isNewRelation(rel), s"Relation $rel multiply declared")
    new Problem(inputRels, inventedRels, outputRels + rel, edb, idb, pos, rules)
  }

  def addOutputRels(rels: Relation*): Problem = {
    val newOutputRels = rels.foldLeft(outputRels) { case (nors, rel) =>
      Contract.require(isNewRelation(rel), s"Relation $rel multiply declared")
      nors + rel
    }
    new Problem(inputRels, inventedRels, newOutputRels, edb, idb, pos, rules)
  }

  private def addTuple(rels: Set[Relation])(config: Config[FValue], rtv: (Relation, DTuple, Double)): Config[FValue] = {
    val (rel, tuple, value) = rtv
    Contract.require(rels.contains(rel), s"Undeclared relation $rel")
    config.add(rel, tuple, FValue(value, Empty))
  }

  def addEDBTuples(rtvs: (Relation, DTuple, Double)*): Problem = {
    val newEDB = rtvs.foldLeft(edb)(addTuple(inputRels)(_, _))
    new Problem(inputRels, inventedRels, outputRels, newEDB, idb, pos, rules)
  }

  def addIDBTuples(rtvs: (Relation, DTuple, Double)*): Problem = {
    val newIDB = rtvs.foldLeft(idb)(addTuple(outputRels)(_, _))
    new Problem(inputRels, inventedRels, outputRels, edb, newIDB, pos, rules)
  }

  def addToken(token: Token, value: Double): Problem = {
    if (pos.contains(token)) {
      scribe.info(s"Ignoring redeclaration of token $token. Already initialized to ${pos(token)}")
      return this
    }
    new Problem(inputRels, inventedRels, outputRels, edb, idb, pos + (token -> value), rules)
  }

  def addRule(rule: Rule): Problem = {
    Contract.require(knownRelation(rule.head.relation))
    Contract.require(rule.body.forall(literal => knownRelation(literal.relation)))
    new Problem(inputRels, inventedRels, outputRels, edb, idb, pos, rules + rule)
  }

  def addRules(newRules: Set[Rule]): Problem = newRules.foldLeft(this)(_ addRule _)

}

object Problem {
  val Empty: Problem = new Problem(Set(), Set(), Set(), Config(), Config(), TokenVec(Map()), Set())
}
