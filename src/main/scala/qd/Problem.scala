package qd

class Problem private (
                        val inputRels: Set[Relation],
                        val inventedRels: Set[Relation],
                        val outputRels: Set[Relation],
                        val edb: Set[(Relation, DTuple, Double)],
                        val idb: Set[(Relation, DTuple, Double)],
                        val pos: TokenVec,
                        val rules: Set[Rule[FValue]]
                      ) {

  val allRels: Set[Relation] = inputRels ++ inventedRels ++ outputRels
  val allTuples: Set[(Relation, DTuple, Double)] = edb ++ idb
  val allTokens: Set[Token] = pos.keySet
  val program = Program("P", rules)

  def edbConfig: Config[FValue] = {
    val edbMap: Map[Relation, Instance[FValue]] = (for (rel <- inputRels)
                                                   yield {
                                                     val tv = edb.filter(_._1 == rel).map(rtv => (rtv._2, rtv._3))
                                                     rel -> tv.foldLeft(Instance[FValue](rel)) { case (inst, (t, v)) =>
                                                       inst + (t -> FValue(v, Empty))
                                                     }
                                                   }).toMap
    Config(edbMap)
  }

  def idbConfig: Config[FValue] = {
    val idbMap: Map[Relation, Instance[FValue]] = (for (rel <- outputRels)
                                                   yield {
                                                     val tv = edb.filter(_._1 == rel).map(rtv => (rtv._2, rtv._3))
                                                     rel -> tv.foldLeft(Instance[FValue](rel)) { case (inst, (t, v)) =>
                                                       inst + (t -> FValue(v, Empty))
                                                     }
                                                   }).toMap
    Config(idbMap)
  }

  def addInputRel(rel: Relation): Problem = {
    require(!allRels.contains(rel), s"Relation $rel multiply declared")
    new Problem(inputRels + rel, inventedRels, outputRels, edb, idb, pos, rules)
  }

  def addInputRels(rels: Relation*): Problem = {
    val newInputRels = rels.foldLeft(inputRels) { case (nirs, rel) =>
      require(!nirs.contains(rel), s"Relation $rel multiply declared")
      nirs + rel
    }
    new Problem(newInputRels, inventedRels, outputRels, edb, idb, pos, rules)
  }

  def addInventedRel(rel: Relation): Problem = {
    require(!allRels.contains(rel), s"Relation $rel multiply declared")
    new Problem(inputRels, inventedRels + rel, outputRels, edb, idb, pos, rules)
  }

  def addInventedRels(rels: Relation*): Problem = {
    val newInventedRels = rels.foldLeft(inventedRels) { case (nirs, rel) =>
      require(!nirs.contains(rel), s"Relation $rel multiply declared")
      nirs + rel
    }
    new Problem(inputRels, newInventedRels, outputRels, edb, idb, pos, rules)
  }

  def addOutputRel(rel: Relation): Problem = {
    require(!allRels.contains(rel), s"Relation $rel multiply declared")
    new Problem(inputRels, inventedRels, outputRels + rel, edb, idb, pos, rules)
  }

  def addOutputRels(rels: Relation*): Problem = {
    val newOutputRels = rels.foldLeft(outputRels) { case (nors, rel) =>
      require(!nors.contains(rel), s"Relation $rel multiply declared")
      nors + rel
    }
    new Problem(inputRels, inventedRels, newOutputRels, edb, idb, pos, rules)
  }

  def addEDBTuple(rt: (Relation, DTuple, Double)): Problem = {
    require(!allTuples.contains(rt), s"Redeclaring tuple $rt")
    new Problem(inputRels, inventedRels, outputRels, edb + rt, idb, pos, rules)
  }

  def addEDBTuples(rts: (Relation, DTuple, Double)*): Problem = {
    val newEDB = rts.foldLeft(edb) { case (db, rt) =>
      require(!allTuples.contains(rt), s"Redeclaring tuple $rt")
      db + rt
    }
    new Problem(inputRels, inventedRels, outputRels, newEDB, idb, pos, rules)
  }

  def addIDBTuple(rt: (Relation, DTuple, Double)): Problem = {
    require(!allTuples.contains(rt), s"Redeclaring tuple $rt")
    new Problem(inputRels, inventedRels, outputRels, edb, idb + rt, pos, rules)
  }

  def addIDBTuples(rts: (Relation, DTuple, Double)*): Problem = {
    val newIDB = rts.foldLeft(idb) { case (db, rt) =>
      require(!allTuples.contains(rt), s"Redeclaring tuple $rt")
      db + rt
    }
    new Problem(inputRels, inventedRels, outputRels, edb, newIDB, pos, rules)
  }

  def addToken(token: Token, value: Double): Problem = {
    require(!pos.contains(token))
    new Problem(inputRels, inventedRels, outputRels, edb, idb, pos + (token -> value), rules)
  }

  def addRule(lineage: Lineage, head: Literal, body: Set[Literal]): Problem = {
    require(allRels.contains(head.relation))
    require(body.forall(literal => allRels.contains(literal.relation)))

    val coeff = pos(lineage)
    val ans = Rule(coeff, head, body).normalize
    val ansNormalized = ans.normalize
    if (rules.contains(ans)) println(s"Warning: Redeclaring rule $ansNormalized")
    new Problem(inputRels, inventedRels, outputRels, edb, idb, pos, rules + ansNormalized)
  }

  def addRules(newRules: Set[Rule[FValue]]): Problem = {
    new Problem(inputRels, inventedRels, outputRels, edb, idb, pos, rules ++ newRules)
  }

}

object Problem {
  def apply(): Problem = new Problem(Set(), Set(), Set(), Set(), Set(), TokenVec(Map()), Set())
}
