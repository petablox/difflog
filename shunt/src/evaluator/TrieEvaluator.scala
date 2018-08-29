package qd
package evaluator

import scala.collection.parallel.ParSeq

case class TE[T <: Value[T]](program: Program[T])(implicit vs: Semiring[T])
extends Evaluator[T]("Trie") {

  val fullTrie: RuleTrie[T] = RuleTrie(program.rules.map(_.normalize))

  override def apply(edb: Config[T]): Config[T] = {
    var (oldConfig, config, delta) = (Config(), edb, edb)
    var i = 0
    while (delta.nonEmptySupport) {
      println(s"Trie evaluator epoch $i")
      val (newConfig, newDelta) = immediateConsequence(config, delta)
      oldConfig = config
      config = newConfig
      delta = newDelta
      i = i + 1
    }
    config
  }

  def immediateConsequence(config: Config[T], delta: Config[T]): (Config[T], Config[T]) = {
    val (newConfig, _, deltaNext) = immediateConsequence(fullTrie, config, delta, Config())
    (newConfig, deltaNext)
  }

  def immediateConsequence(
                            trie: RuleTrie[T],
                            config: Config[T],
                            deltaCurr: Config[T],
                            deltaNext: Config[T]
                          ): (Config[T], Config[T], Config[T]) = {
    immediateConsequence(deltaDone = false, ParSeq(), trie, config, deltaCurr, deltaNext)
  }

  def immediateConsequence(
                            deltaDone: Boolean,
                            assignments: ParSeq[Assignment[T]],
                            trie: RuleTrie[T],
                            config: Config[T],
                            deltaCurr: Config[T],
                            deltaNext: Config[T]
                          ): (Config[T], Config[T], Config[T]) = {
    var (newConfig, newDeltaCurr, newDeltaNext) = (config, deltaCurr, deltaNext)

    // Step 1: Process sub-tries
    for (literal <- trie.map.keys) {
      if (!deltaDone) {
        val subAssignments1 = extend(literal, newDeltaCurr, assignments)
        val ncdcdn = immediateConsequence(deltaDone = true,
                                          subAssignments1,
                                          trie.map(literal),
                                          newConfig,
                                          newDeltaCurr,
                                          newDeltaNext)
        newConfig = ncdcdn._1; newDeltaCurr = ncdcdn._2; newDeltaNext = ncdcdn._3
      }

      val subAssignments2 = extend(literal, newConfig, assignments)
      val ncdcdnp = immediateConsequence(deltaDone,
                                         subAssignments2,
                                         trie.map(literal),
                                         newConfig,
                                         newDeltaCurr,
                                         newDeltaNext)
      newConfig = ncdcdnp._1; newDeltaCurr = ncdcdnp._2; newDeltaNext = ncdcdnp._3
    }

    // Step 2: Process current leaves
    if (deltaDone) {
      for (rule <- trie.leaves) {
        val newTuples = assignments.map(_ * rule.coeff).map(_.toTuple(rule.head)).toMap

        val relation = rule.head.relation
        val oldInstance = config(relation)
        val newInstance = newTuples.foldLeft(oldInstance)(_ + _)
        newConfig = config + (relation -> newInstance)

        val deltaTuples = newTuples.filter { case (tuple, value) => value > oldInstance(tuple) }

        val dcr = newDeltaCurr(relation)
        val ndcr = deltaTuples.foldLeft(dcr)(_ + _)
        newDeltaCurr = newDeltaCurr + (relation -> ndcr)

        val dnr = newDeltaNext(relation)
        val ndnr = deltaTuples.foldLeft(dnr)(_ + _)
        newDeltaNext = newDeltaNext + (relation -> ndnr)
      }
    }

    (newConfig, newDeltaCurr, newDeltaNext)
  }

  def extend(literal: Literal, config: Config[T], bodyVals: ParSeq[Assignment[T]]): ParSeq[Assignment[T]] = {
    for (assignment <- bodyVals;
         f = assignment.toFilter(literal);
         (tuple, score) <- config(literal.relation).filter(f);
         newValuation <- extend(literal, tuple, assignment))
      yield newValuation * score
  }

  def extend(literal: Literal, tuple: DTuple, assignment: Assignment[T]): Option[Assignment[T]] = {
    var ans = assignment
    for ((par, field) <- literal.fields.zip(tuple)) {
      par match {
        case v @ Variable(_, _) =>
          if (!ans.contains(v)) ans = ans + (v -> field)
          else if (ans(v) != field) return None
        case Constant(c, _) => if (c != field) return None
      }
    }
    Some(ans)
  }

}
