package qd
package problem

import scala.util.Random

class ALPSParser {

  def parse(dataStr: String, templateStr: String): Problem = {
    var problem = Problem.Empty
    problem = parseDataStr(problem, dataStr)
    problem = parseTemplateStr(problem, templateStr)

    scribe.info(s"Input relations: ${problem.inputRels.size}")
    scribe.info(s"Invented relations: ${problem.inventedRels.size}")
    scribe.info(s"Output relations: ${problem.outputRels.size}")
    scribe.info(s"Rules: ${problem.rules.size}")

    problem
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // 1. Parse Training Data

  def parseDataStr(problem: Problem, dataStr: String): Problem = {
    var ans = problem
    var lines = dataStr.split(System.lineSeparator()).map(_.trim)

    //////////
    // 1a. Read domain declarations

    var domainNames = Map[String, Domain]()
    var domainElems = Map[Domain, Set[Constant]]()
    def domainDeclarationsPred(str: String): Boolean = str.contains(":")
    val domainDeclarations = lines.takeWhile(domainDeclarationsPred)
    lines = lines.dropWhile(domainDeclarationsPred)
    assert(lines.forall(str => !domainDeclarationsPred(str)))
    lines = lines.dropWhile(_.isEmpty)

    for (domainDecl <- domainDeclarations) {
      val Array(nameStr, es) = domainDecl.split(":")

      val name = nameStr.trim
      assert(name.nonEmpty)
      assert(!domainNames.contains(name))
      val domain = Domain(name)

      assert(es.endsWith("."))
      val es2 = es.dropRight(1).split(",").map(_.trim)
      assert(es2.forall(_.nonEmpty))
      val elems = es2.map(elem => Constant(elem, domain)).toSet

      domainNames = domainNames + (name -> domain)
      domainElems = domainElems + (domain -> elems)
    }

    //////////
    // 1b. Read relations

    while (lines.nonEmpty) {
      val signature = lines.head
      lines = lines.tail.dropWhile(l => l.isEmpty || l == ";")

      // 1b(i). Parse relation declaration
      val isEDB = signature.startsWith("*")
      val s2 = if (isEDB) signature.tail.trim else signature
      val s2s = s2.split("\\(|,|\\)").map(_.trim)
      val name = s2s.head
      val domains = s2s.tail.map(d => domainNames(d)).toVector
      val relation = Relation(name, domains)

      if (isEDB) {
        ans = ans.addInputRel(relation)
      } else {
        ans = ans.addOutputRel(relation)
      }

      // 1b(ii). Parse tuples present in relation
      while (lines.head != ".") {
        val fieldStrs = lines.head.split(",").map(_.trim)
        lines = lines.tail.dropWhile(l => l.isEmpty || l == ";")

        assert(fieldStrs.length == domains.size)
        val fields = domains.zip(fieldStrs).map { case (dom, v) => domainElems(dom).find(_ == Constant(v, dom)).get }
        val t = DTuple(fields)

        if (isEDB) {
          ans = ans.addEDBTuples((relation, t, 1.0))
        } else {
          ans = ans.addIDBTuples((relation, t, 1.0))
        }
      }

      assert(lines.head == ".")
      lines = lines.tail.dropWhile(_.isEmpty)
    }

    ans
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // 2. Parse Template Rules

  val rng: Random = Random
  var numTokens = 0
  def nextToken(): Token = { val ans = numTokens; numTokens = numTokens + 1; Token(s"R$ans") }

  def parseTemplateStr(problem: Problem, templateStr: String): Problem = {
    var ans = problem

    val templateLines = templateStr.split(System.lineSeparator()).map(_.trim).filter(_.nonEmpty)
    for (templateLine <- templateLines) {
      assert(templateLine.contains(":-") && templateLine.endsWith(".") && !templateLine.contains("\t"))
      val tl2 = templateLine.dropRight(1).trim
                            .replaceAll("\\),", ")\t")
                            .replaceAll(" :- ", "\t")
      val mlitStrs = tl2.split("\t").map(_.trim).filter(_.nonEmpty)
      val mlits = for (mlitStr <- mlitStrs.toVector)
                  yield {
                    val ms2 = mlitStr.split("\\(|,|\\)").map(_.trim).filter(_.nonEmpty)
                    (ms2.head, ms2.tail.toVector)
                  }
      assert(mlitStrs.length >= 2)

      for ((bodyLits, msr, msv) <- instantiateMetaLiterals(mlits.tail, problem);
           (headLit, _, _) <- instantiateMetaLiteral(mlits.head, msr, msv, problem.outputRels ++
                                                                           problem.inventedRels)) {
        val token = nextToken()
        val value = FValue(rng.nextDouble(), token)
        val rule = Rule(value, headLit, bodyLits.toVector).normalized
        ans = ans.addRule(rule)
      }
    }

    ans
  }

  def instantiateMetaLiterals(
                               mlits: IndexedSeq[(String, IndexedSeq[String])],
                               problem: Problem
                             ): Set[(Set[Literal], Map[String, Relation], Map[String, Variable])] = {
    if (mlits.isEmpty) {
      Set((Set(), Map(), Map()))
    } else {
      for ((lits, msr, msv) <- instantiateMetaLiterals(mlits.tail, problem);
           (l2, msr2, msv2) <- instantiateMetaLiteral(mlits.head, msr, msv, problem.inputRels ++
                                                                            problem.inventedRels ++
                                                                            problem.outputRels))
      yield (lits + l2, msr2, msv2)
    }
  }

  def instantiateMetaLiteral(
                              mlit: (String, IndexedSeq[String]),
                              relMap: Map[String, Relation],
                              varMap: Map[String, Variable],
                              relations: Set[Relation]
                            ): Set[(Literal, Map[String, Relation], Map[String, Variable])] = {
    val (relName, fieldNames) = mlit
    val availableRels = if (relMap.contains(relName)) Set(relMap(relName))
                        else relations.filter(_.arity == fieldNames.length)
    for (relation <- availableRels; (varInst, vm2) <- instantiateMetaVars(fieldNames, relation.signature, varMap))
    yield (Literal(relation, varInst), relMap + (relName -> relation), vm2)
  }

  def instantiateMetaVars(
                           varNames: IndexedSeq[String],
                           signature: IndexedSeq[Domain],
                           varMap: Map[String, Variable]
                         ): Option[(Vector[Variable], Map[String, Variable])] = {
    require(varNames.size == signature.size)
    if (varNames.isEmpty) {
      Some((Vector(), varMap))
    } else instantiateMetaVars(varNames.tail, signature.tail, varMap) match {
      case Some((vvt, vmt)) =>
        if (vmt.contains(varNames.head)) {
          if (vmt(varNames.head).domain == signature.head) Some((vmt(varNames.head) +: vvt, vmt))
          else None
        } else {
          val vhead = Variable(varNames.head, signature.head)
          Some((vhead +: vvt, vmt + (varNames.head -> vhead)))
        }
      case None => None
    }
  }

}
