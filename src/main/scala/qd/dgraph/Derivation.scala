package qd
package dgraph

import qd.tokenvec.TokenVec
import qd.util.{Contract, Random}

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

  /* def sample(graph: DGraph, relation: Relation, tuple: DTuple, pos: TokenVec): Lineage = {
    val derivations = graph(relation)(tuple)
    if (derivations.size == 1 && derivations.head.isInstanceOf[EDB]) {
      Empty
    } else {
      val clauses = derivations.map(_.asInstanceOf[Clause])
      val weightedClauses = clauses.map(clause => (clause, Value(clause.rule.lineage, pos).v))
      val hi = weightedClauses.map(_._2).sum
      val coin = Random.nextDouble(lo = 0, hi)

      var remainingClauses = weightedClauses.toVector
      while (coin > remainingClauses.head._2) {
        remainingClauses = remainingClauses.tail
      }
      val chosenClause = remainingClauses.head._1

      var ans = chosenClause.rule.lineage
      for (i <- chosenClause.rule.body.indices) {
        ans = ans * sample(graph, chosenClause.rule.body(i).relation, chosenClause.antecedents(i), pos)
      }
      ans
    }
  } */

  def sample(graph: DGraph, relation: Relation, tuple: DTuple, pos: TokenVec): Lineage = {
    sample(graph, relation, tuple, Set(), Set(), pos).get._1
  }

  def sample(
              graph: DGraph,
              relation: Relation,
              tuple: DTuple,
              justified: Set[(Relation, DTuple)],
              stack: Set[(Relation, DTuple)],
              pos: TokenVec
            ): Option[(Lineage, Set[(Relation, DTuple)])] = {
    if (justified.contains((relation, tuple))) {
      Some((Empty, justified))
    } else if (stack.contains((relation, tuple))) {
      None
    } else {
      val derivations = graph(relation)(tuple)
      if (derivations.size == 1 && derivations.head.isInstanceOf[EDB]) {
        Some((Empty, justified + ((relation, tuple))))
      } else {
        var remainingDerivations = derivations.map(_.asInstanceOf[Clause])
        val sj = stack + ((relation, tuple))
        while (remainingDerivations.nonEmpty) {
          val clauses = remainingDerivations
          val weightedClauses = clauses.map(clause => (clause, Value(clause.rule.lineage, pos).v))
          val hi = weightedClauses.map(_._2).sum
          val coin = Random.nextDouble(lo = 0, hi)

          val chosenClause = {
            var remainingClauses = weightedClauses.toVector
            while (coin > remainingClauses.head._2) {
              remainingClauses = remainingClauses.tail
            }
            remainingClauses.head._1
          }

          var success = true
          var ansl = chosenClause.rule.lineage
          var ansj = justified
          for (i <- chosenClause.rule.body.indices if success) {
            sample(graph, chosenClause.rule.body(i).relation, chosenClause.antecedents(i), ansj, sj, pos) match {
              case Some((lt, jt)) =>
                ansl = ansl * lt
                ansj = jt
              case None =>
                success = false
                remainingDerivations = remainingDerivations - chosenClause
            }
          }
          if (success) return Some((ansl, ansj))
        }
        None
      }
    }
  }

  def samplePath(graph: DGraph, relation: Relation, tuple: DTuple, pos: TokenVec): Vector[Clause] = {
    samplePath(graph, relation, tuple, Set(), pos).get
  }

  def samplePath(
                  graph: DGraph,
                  relation: Relation,
                  tuple: DTuple,
                  stack: Set[(Relation, DTuple)],
                  pos: TokenVec
                ): Option[Vector[Clause]] = {
    if (stack.contains((relation, tuple))) {
      None
    } else {
      val derivations = graph(relation)(tuple)
      if (derivations.size == 1 && derivations.head.isInstanceOf[EDB]) {
        // the current tuple is an EDB, then job is done
        Some(Vector())
      } else {
        var remainingDerivations = derivations.map(_.asInstanceOf[Clause])
        val sj = stack + ((relation, tuple))
        // repeat sampling until a success
        while (remainingDerivations.nonEmpty) {

          // randomly sample a derivation
          val clauses = remainingDerivations
          val weightedClauses = clauses.map(clause => (clause, Value(clause.rule.lineage, pos).v))
          val hi = weightedClauses.map(_._2).sum
          val coin = Random.nextDouble(lo = 0, hi)

          val chosenClause = {
            var remainingClauses = weightedClauses.toVector
            while (coin > remainingClauses.head._2) {
              remainingClauses = remainingClauses.tail
            }
            remainingClauses.head._1
          }

          // randomly sample one hypothese from the sampled derivation
          var remainingHypotheses = chosenClause.rule.body.map(_.relation).zip(chosenClause.antecedents).filterNot(stack)
          while (remainingHypotheses.nonEmpty) {
            val chosenHypothesisIndex = Random.nextInt(lo = 0, remainingHypotheses.size)
            val chosenHypothesis = remainingHypotheses(chosenHypothesisIndex)
            samplePath(graph, chosenHypothesis._1, chosenHypothesis._2, sj, pos) match {
              case Some(cl) => return Some(cl :+ chosenClause)
              case None =>
                remainingHypotheses = remainingHypotheses.filterNot(_ == chosenHypothesis)
            }
          }

          remainingDerivations = remainingDerivations - chosenClause
        }
        None
      }
    }
  }

}