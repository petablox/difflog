package qd
package learner

import qd.Semiring.FValueSemiringObj
import qd.evaluator.{Evaluator, TrieEvaluator}

class Learner(q0: Problem) {

  implicit val vs: FValueSemiring = FValueSemiringObj

  val edb: Config[FValue] = q0.edb.foldLeft(Config()) { case (config, (relation, tuple)) =>
    config.add(relation, tuple, vs.One)
  }
  val referenceIDB: Config[FValue] = q0.idb.foldLeft(Config()) { case (config, (relation, tuple)) =>
    config.add(relation, tuple, vs.One)
  }

  val tokens: Set[Token] = q0.allTokens
  val evaluator: Evaluator = TrieEvaluator
  val scorer = new Scorer(edb, referenceIDB, evaluator)

  private var pos: TokenVec = q0.pos
  private var program: Program[FValue] = q0.program
  private var currentIDB: Config[FValue] = evaluator(program, edb)
  private var bestIDB: Option[(TokenVec, Program[FValue], Config[FValue], Double)] = None
  private var step: TokenVec = TokenVec(tokens.map(token => token -> 1.0).toMap)

  def getPos: TokenVec = pos
  def getProgram: Program[FValue] = program
  def getCurrentIDB: Config[FValue] = currentIDB
  def getBestIDB: (TokenVec, Program[FValue], Config[FValue], Double) = bestIDB.get

  def learn(tgtLoss: Double, maxIters: Int): (TokenVec, Program[FValue], Config[FValue], Double) = {
    require(maxIters > 0)
    var (l2, numIters, gradAbs) = (tgtLoss + 1, 0, 1.0)
    while (numIters < maxIters && l2 >= tgtLoss && gradAbs > 0 && step.abs > 0.0) {
      update()
      l2 = scorer.errorL2(currentIDB)
      numIters += 1
      gradAbs = scorer.gradL2(pos, currentIDB).abs
    }
    println(s"#Iterations: $numIters")
    getBestIDB
  }

  def update(): Unit = {
    val oldPos = pos
    pos = newPosL2Newton
    program = pos(program)
    currentIDB = evaluator(program, edb)
    step = pos - oldPos

    val l2 = scorer.errorL2(currentIDB)
    if (bestIDB.isEmpty || l2 < bestIDB.get._4) bestIDB = Some((pos, program, currentIDB, l2))
  }

  def newPosL2Newton: TokenVec = {
    val l2 = scorer.errorL2(currentIDB)
    val grad = scorer.gradL2(pos, currentIDB)

    // if (grad.abs == 0) { this }
    val delta = grad.unit * l2 / grad.abs
    val newPos = pos - delta
    val newPosLim = newPos.limitLower(0.0).limitLower(0.01, pos).limitUpper(0.99, pos).limitUpper(1.0)
    val step = newPosLim - pos
    val bestL2 = bestIDB.map(_._4).getOrElse(Double.PositiveInfinity)
    // println(s"  grad: $grad")
    println(s"  l2: $l2. best.l2: $bestL2. |grad|: ${grad.abs}. |step|: ${step.abs}.")
    newPosLim
  }

}
