package qd
package dgraph

import qd.util.Contract

sealed abstract class Derivation {
  def conclusion: DTuple
}
case class EDB(conclusion: DTuple) extends Derivation
case class Clause(conclusion: DTuple, rule: Rule, antecedents: IndexedSeq[DTuple]) extends Derivation {
  Contract.require(conclusion.signature == rule.head.relation.signature)
  Contract.require(rule.body.length == antecedents.length)
  Contract.require(rule.body.indices.forall { i =>
    rule.body(i).relation.signature == antecedents(i).signature
  })
}

object Derivation {
  type DGraph = Map[Relation, Map[DTuple, Set[Derivation]]]
  def apply(conclusion: DTuple): Derivation = EDB(conclusion)
}