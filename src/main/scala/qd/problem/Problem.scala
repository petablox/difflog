package qd
package problem

import qd.Semiring.FValueSemiringObj
import qd.instance.{Config, Instance}
import util.Contract

class Problem private (
                        val inputRels: Set[Relation],
                        val inventedRels: Set[Relation],
                        val outputRels: Set[Relation],

                        val discreteEDB: Map[Relation, Set[DTuple]],
                        val discreteIDB: Map[Relation, Set[DTuple]],

                        val pos: TokenVec,
                        val rules: Set[Rule]
                      ) {

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // 1. Relations

  ////
  // 1a. Accessors

  def findRel(pred: Relation => Boolean): Option[Relation] = {
    val f1 = inputRels.find(pred)
    val f2 = inventedRels.find(pred)
    val f3 = outputRels.find(pred)
    f1 orElse f2 orElse f3
  }
  def knownRelation(rel: Relation): Boolean = findRel(_ == rel).nonEmpty
  def isNewRelation(rel: Relation): Boolean = !knownRelation(rel)

  ////
  // 1b. Modifiers

  private def addRel(rels: Set[Relation], newRel: Relation): Set[Relation] = {
    Contract.require(isNewRelation(newRel), s"Relation $newRel multiply declared")
    rels + newRel
  }

  def addInputRel(rel: Relation): Problem = {
    new Problem(addRel(inputRels, rel), inventedRels, outputRels, discreteEDB + (rel -> Set()), discreteIDB, pos, rules)
  }

  def addInventedRel(rel: Relation): Problem = {
    new Problem(inputRels, addRel(inventedRels, rel), outputRels, discreteEDB, discreteIDB, pos, rules)
  }

  def addOutputRel(rel: Relation): Problem = {
    new Problem(inputRels, inventedRels, addRel(outputRels, rel), discreteEDB, discreteIDB + (rel -> Set()), pos, rules)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // 2. EDB and IDB

  ////
  // 2a. Discrete EDB to Configuration

  def discreteTuplesToInstance(relation: Relation, tuples: Set[DTuple]): Instance[FValue] = {
    tuples.foldLeft(Instance(relation.signature)) { case (instance, t) =>
      instance + (t -> FValueSemiringObj.One)
    }
  }

  lazy val edb: Config[FValue] = Config(discreteEDB.map { case (rel, ts) => rel -> discreteTuplesToInstance(rel, ts) })
  lazy val idb: Config[FValue] = Config(discreteIDB.map { case (rel, ts) => rel -> discreteTuplesToInstance(rel, ts) })

  ////
  // 2b. Adding tuples

  private def addTuples(
                         config: Map[Relation, Set[DTuple]],
                         rts: collection.immutable.Seq[(Relation, DTuple)]
                       ): Map[Relation, Set[DTuple]] = {
    rts.foldLeft(config) { case (cfg, (rel, tuple)) =>
      Contract.require(config.contains(rel), s"Undeclared relation $rel")
      cfg + (rel -> (config(rel) + tuple))
    }
  }

  def addEDBTuples(rts: collection.immutable.Seq[(Relation, DTuple)]): Problem = {
    new Problem(inputRels, inventedRels, outputRels, addTuples(discreteEDB, rts), discreteIDB, pos, rules)
  }

  def addIDBTuples(rts: collection.immutable.Seq[(Relation, DTuple)]): Problem = {
    new Problem(inputRels, inventedRels, outputRels, discreteEDB, addTuples(discreteIDB, rts), pos, rules)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // 3. Tokens and Rules

  def allTokens: Set[Token] = pos.keySet

  def addToken(token: Token, value: Double): Problem = {
    if (pos.contains(token)) {
      scribe.info(s"Ignoring redeclaration of token $token. Already initialized to ${pos(token)}")
      return this
    }
    new Problem(inputRels, inventedRels, outputRels, discreteEDB, discreteIDB, pos + (token -> value), rules)
  }

  def addRule(rule: Rule): Problem = {
    Contract.require(knownRelation(rule.head.relation))
    Contract.require(rule.body.forall(literal => knownRelation(literal.relation)))
    new Problem(inputRels, inventedRels, outputRels, discreteEDB, discreteIDB, pos, rules + rule)
  }

  def addRules(newRules: Set[Rule]): Problem = newRules.foldLeft(this)(_ addRule _)

}

object Problem {
  val Empty: Problem = new Problem(Set(), Set(), Set(), Map(), Map(), TokenVec(Map()), Set())
}
