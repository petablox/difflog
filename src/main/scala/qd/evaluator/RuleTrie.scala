package qd
package evaluator

case class RuleTrie[T <: Value[T]](leaves: Set[Rule[T]], map: Map[Literal, RuleTrie[T]]) extends Iterable[Rule[T]] {

  // Commented because the following check is too time-consuming
  // require(map.forall { case (literal, trie) => trie.forall(_.body.contains(literal)) })

  val numRules: Int = leaves.size + map.values.map(_.numRules).sum
  override def iterator: Iterator[Rule[T]] = map.values.foldLeft(leaves.iterator)(_ ++ _.iterator)
  val variables: Set[Variable] = {
    val vs1 = leaves.flatMap(_.head.variables)
    val vs2 = map.flatMap { case (l, t) => l.variables ++ t.variables }
    vs1 ++ vs2
  }

  def +(rule: Rule[T]): RuleTrie[T] = {
    def add(remainingLiterals: Seq[Literal], trie: RuleTrie[T]): RuleTrie[T] = {
      if (remainingLiterals.isEmpty) RuleTrie(trie.leaves + rule, trie.map)
      else {
        val litHead = remainingLiterals.head
        val litRest = remainingLiterals.tail
        val subTrie = trie.map.getOrElse(litHead, RuleTrie())
        RuleTrie(trie.leaves, trie.map + (litHead -> add(litRest, subTrie)))
      }
    }
    add(rule.body.toSeq.sortBy(_.toString), this)
  }

}

object RuleTrie {
  def apply[T <: Value[T]](): RuleTrie[T] = RuleTrie(Set(), Map())
  def apply[T <: Value[T]](rules: Iterable[Rule[T]]): RuleTrie[T] = rules.foldLeft(RuleTrie[T]())(_ + _)
}
