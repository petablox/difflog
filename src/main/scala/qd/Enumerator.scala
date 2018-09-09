package qd

import scala.collection.immutable.Seq

object Enumerator {

  def normalize(lits: Set[Literal]): (Set[Literal], Map[Variable, Variable]) = {
    val renaming = scala.collection.mutable.Map[Variable, Variable]()
    def rename(vOld: Variable): Variable = {
      if (!renaming.contains(vOld)) {
        val index = Stream.from(0).find(idx => renaming.values.count(_.name == s"v$idx") == 0).get
        renaming.put(vOld, Variable(s"v$index", vOld.domain))
      }
      renaming(vOld)
    }

    val oldLiterals = lits.toSeq.sortBy(_.toString)
    val newLiterals = for (oldLit <- oldLiterals)
      yield {
        val newFields = oldLit.fields.map {
          case v @ Variable(_, _) => rename(v)
          case p @ Constant(_, _) => p
        }
        Literal(oldLit.relation, newFields)
      }

    (newLiterals.toSet, renaming.toMap)
  }

  def normalize[T <: Value[T]](rule: Rule[T]): Rule[T] = {
    val (newBody, renaming) = normalize(rule.bodySet)

    val oldHead = rule.head
    val newHeadFields = oldHead.fields.map {
      case v @ Variable(_, _) => renaming(v)
      case c @ Constant(_, _) => c
    }
    val newHead = Literal(oldHead.relation, newHeadFields)

    Rule(rule.coeff, newHead, newBody.toList.sortBy(_.toString))
  }

  def skeleton[T <: Value[T]](
                               inputRels: Set[Relation], inventedRels: Set[Relation], outputRels: Set[Relation],
                               weight: (Literal, Seq[Literal]) => (Token, T),
                               maxLiterals: Int, maxVars: Int
                             )(implicit vs: Semiring[T]): (Map[Token, T], Set[Rule[T]]) = {

    require(inputRels.intersect(inventedRels).isEmpty)
    require(inputRels.intersect(outputRels).isEmpty)
    require(inventedRels.intersect(outputRels).isEmpty)
    require(maxLiterals >= 0 && maxVars >= 0)

    val allRels = inputRels ++ inventedRels ++ outputRels

    def allLiteralSets(length: Int, freeVars: Set[Variable]): Set[Set[Literal]] = {
      require(length >= 0)
      if (length == 0) Set(Set())
      else {
        def allLiterals(hypRel: Relation): Set[(Literal, Set[Variable])] = {
          def allBindings(signature: Seq[Domain], fvp: Set[Variable]): Set[Seq[Variable]] = {
            if (signature.isEmpty) Set(Seq())
            else {
              val domHead = signature.head
              def newVar(): Variable = {
                var index = 0
                while (fvp.contains(Variable(s"v$index", domHead))) index = index + 1
                Variable(s"v$index", domHead)
              }
              var availableVars = if (fvp.size < maxVars) fvp + newVar() else fvp
              availableVars = availableVars.filter(_.domain == domHead)
              for (bhead <- availableVars; brest <- allBindings(signature.tail, fvp + bhead))
              yield bhead +: brest
            }
          }
          for (binding <- allBindings(hypRel.signature, freeVars))
          yield (Literal(hypRel, binding), freeVars ++ binding)
        }
        for (hypRel <- allRels;
             (lit, fvp) <- allLiterals(hypRel);
             lits <- allLiteralSets(length - 1, fvp))
        yield lits + lit
      }
    }

    val allBodies = (0 to maxLiterals).flatMap(length => allLiteralSets(length, Set()))
                                      .map(litSet => normalize(litSet)._1)
                                      .toSet

    def allHeads(targetRel: Relation, body: Set[Literal]): Set[Literal] = {
      val allVars = body.flatMap(_.variables)
      def allBindings(signature: Seq[Domain]): Set[Seq[Variable]] = {
        if (signature.isEmpty) Set(Seq())
        else {
          val availableVars = allVars.filter(_.domain == signature.head)
          for (bhead <- availableVars; brest <- allBindings(signature.tail))
          yield bhead +: brest
        }
      }
      for (binding <- allBindings(targetRel.signature))
      yield Literal(targetRel, binding)
    }

    def reachableVars(rule: Rule[T]): Set[Variable] = {
      var (ans, candidate) = (Set[Variable](), rule.head.variables)
      while (ans != candidate) {
        ans = candidate
        candidate = for (lit <- rule.bodySet if lit.variables.intersect(ans).nonEmpty; v <- lit.variables)
                    yield v
      }
      ans
    }

    def isDegenerate(rule: Rule[T]): Boolean = {
      val head = rule.head
      val body = rule.body
      body.contains(head) || reachableVars(rule).size < rule.variables.size
    }

    val unweightedRules = for (targetRel <- inventedRels ++ outputRels;
                               bodySet <- allBodies;
                               body = bodySet.toList.sortBy(_.toString);
                               head <- allHeads(targetRel, bodySet);
                               rule = Rule(vs.One, head, body)
                               if !isDegenerate(rule))
                          yield rule

    val weightedTriples = for (uwrule <- unweightedRules)
                          yield {
                            val tv = weight(uwrule.head, uwrule.body)
                            val rule = Rule(tv._2, uwrule.head, uwrule.body)
                            (tv._1, tv._2, rule)
                          }
    val pos = weightedTriples.map(triple => triple._1 -> triple._2).toMap
    val weightedRules: Set[Rule[T]] = weightedTriples.map(_._3)

    (pos, weightedRules)

  }

}
