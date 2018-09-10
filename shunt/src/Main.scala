  val vs: Semiring[FValue] = implicitly[Semiring[FValue]]

  def eval(): Unit = {
    val problem = readProblem(args(1), ???, ???)
    val hipos = TokenVec.one(problem.allTokens)
    val hirules = hipos(problem.rules)
    val scorer = new L2Scorer(problem.edbConfig, problem.idbConfig, TrieEvaluator)
    val idb = TrieEvaluator(hirules, problem.edbConfig)

    val idbStr = problem.outputRels.flatMap(rel =>
      idb(rel).support.toSeq.sortBy(-_._2.v).map { case (t, v) => s"  ${v.v}: ${rel.name}$t" }
    ).mkString("," + System.lineSeparator())

    println("IDB {")
    println(idbStr)
    println("}")
    println(s"// F1: ${scorer.f1(idb, 0.5)}")
  }

  def learn(): Unit = {
    val (trainFile, tgtLoss, maxIters, testFile) = (args(1), args(2).toDouble, args(3).toInt, args(4))

    require(0 <= tgtLoss && tgtLoss <= 1.0, s"Expected target loss between 0.0 and 1.0: Found $tgtLoss")
    require(maxIters > 0, s"Expected maxIters > 0: Found $maxIters")

    val trainProblem = readProblem(trainFile, ???, ???)
    val learner = new Learner(trainProblem)
    val result = learner.learn(tgtLoss, maxIters)
    assert(result._4 < tgtLoss)
    learner.reinterpret

    val testProblemString = Source.fromFile(testFile).mkString
    val testParser = new DifflogParser(???, ???)
    val testProblem = testParser.parseAll(testParser.problem, testProblemString).get

    ???
  }

  def tab2(): Unit = {
    val (trainFile, tgtLoss, maxIters, testFile) = (args(1), args(2).toDouble, args(3).toInt, args(4))

    require(0 <= tgtLoss && tgtLoss <= 1.0, s"Expected target loss between 0.0 and 1.0: Found $tgtLoss")
    require(maxIters > 0, s"Expected maxIters > 0: Found $maxIters")

    val trainProblem = readProblem(trainFile, ???, ???)
    val learner = new Learner(trainProblem)
    learner.learn(tgtLoss, maxIters)
    // learner.keepUseful
    val reinterpretedResult = learner.reinterpret

    /* println("---")
    val rr = reinterpretedResult.minBy(_._4)
    for (rel <- trainProblem.outputRels;
         allTuples = trainProblem.idbConfig(rel).support.map(_._1) ++ rr._3(rel).support.map(_._1);
         t <- allTuples) {
      val vRef = trainProblem.edbConfig(rel)(t)
      val vProd = rr._3(rel)(t)
      if (vRef != vProd) { println(s"$rel $t $vRef $vProd") }
    }
    println("---") */

    val testProblem = readProblem(testFile, ???, ???)
    val testScorer = new L2Scorer(testProblem.edbConfig, testProblem.idbConfig, TrieEvaluator)
    for ((_, prog, _, trainF2) <- reinterpretedResult) {
      val testIDB = TrieEvaluator(prog, testProblem.edbConfig)
      val testF2 = testScorer.f1(testIDB, 0.5)
      println(s"${prog.name} $trainF2 $testF2")

      /* if (trainLoss == 0 && testLoss > 0) {
        for (rel <- testScorer.outputRels;
             allTuples = testScorer.refIDB(rel).support.map(_._1) ++ testIDB(rel).support.map(_._1);
             t <- allTuples;
             vRef = testScorer.refIDB(rel)(t);
             vProd = testIDB(rel)(t)
             if vRef.v != vProd.v) {
          println(s"$rel $t $vRef $vProd")
        }
      } */
    }
  }

  if (args(0) == "eval") { eval() }
  else if (args(0) == "learn") { learn() }
  else if (args(0) == "tab2") { tab2() }
  else throw new UnsupportedOperationException(s"Unrecognized command ${args(0)}")
