package qd

class Problem private (
                        val inputRels: Set[Relation],
                        val inventedRels: Set[Relation],
                        val outputRels: Set[Relation],
                        val edb: Set[(Relation, DTuple)],
                        val idb: Set[(Relation, DTuple)],
                        val pos: TokenVec,
                        val rules: Set[Rule[FValue]]
                      ) {

  val allRels: Set[Relation] = inputRels ++ inventedRels ++ outputRels
  val allTuples: Set[(Relation, DTuple)] = edb ++ idb
  val allTokens: Set[Token] = pos.keySet
  val program = Program("P", rules)

  def addInputRel(rel: Relation): Problem = {
    require(!allRels.contains(rel), s"Relation $rel multiply declared")
    new Problem(inputRels + rel, inventedRels, outputRels, edb, idb, pos, rules)
  }

  def addInventedRel(rel: Relation): Problem = {
    require(!allRels.contains(rel), s"Relation $rel multiply declared")
    new Problem(inputRels, inventedRels + rel, outputRels, edb, idb, pos, rules)
  }

  def addOutputRel(rel: Relation): Problem = {
    require(!allRels.contains(rel), s"Relation $rel multiply declared")
    new Problem(inputRels, inventedRels, outputRels + rel, edb, idb, pos, rules)
  }

  def addEDBTuple(rt: (Relation, DTuple)): Problem = {
    require(!allTuples.contains(rt), s"Redeclaring tuple $rt")
    new Problem(inputRels, inventedRels, outputRels, edb + rt, idb, pos, rules)
  }

  def addIDBTuple(rt: (Relation, DTuple)): Problem = {
    require(!allTuples.contains(rt), s"Redeclaring tuple $rt")
    new Problem(inputRels, inventedRels, outputRels, edb, idb + rt, pos, rules)
  }

  def addToken(token: Token, value: Double): Problem = {
    require(!pos.contains(token))
    new Problem(inputRels, inventedRels, outputRels, edb, idb, pos + (token -> value), rules)
  }

  def addRule(lineage: Lineage, head: Literal, body: Set[Literal]): Problem = {
    require(allRels.contains(head.relation))
    require(body.forall(literal => allRels.contains(literal.relation)))

    val coeff = pos(lineage)
    val ans = Rule(coeff, head, body)
    val ansNormalized = ans.normalize
    if (rules.contains(ansNormalized)) println(s"Warning: Redeclaring rule $ans")
    new Problem(inputRels, inventedRels, outputRels, edb, idb, pos, rules + ansNormalized)
  }

}

object Problem {
  def apply(): Problem = new Problem(Set(), Set(), Set(), Set(), Set(), TokenVec(Map()), Set())
}
