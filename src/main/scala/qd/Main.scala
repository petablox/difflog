package qd

import qd.Semiring.FValueSemiringObj
import qd.evaluator.TrieSemiEvaluator

object Main extends App {

  implicit val vs: FValueSemiring = FValueSemiringObj

  val parser = new Parser()
  val input = scala.io.Source.fromInputStream(System.in).mkString
  val problem = parser.parseAll(parser.problem, input).get

  def eval(): Unit = {
    val edbMap: Map[Relation, Instance[FValue]] = (for (rel <- problem.inputRels)
                                                   yield {
                                                     val tuples = problem.edb.filter(_._1 == rel).map(_._2)
                                                     rel -> tuples.foldLeft(Instance(rel)) { case (inst, t) =>
                                                       inst + (t -> vs.One)
                                                     }
                                                   }).toMap
    val edb: Config[FValue] = Config(edbMap)
    val idb = TrieSemiEvaluator(problem.rules, edb)
    val idbStr = problem.outputRels.flatMap(rel =>
      idb(rel).support.toSeq.sortBy(-_._2.v).map { case (t, v) => s"  ${v.v}: ${rel.name}$t" }
    ).mkString("," + System.lineSeparator())

    println("IDB {")
    println(idbStr)
    println("}")
  }

  def learn(): Unit = {
    ???
  }

  if (args(0) == "eval") { eval() }
  else if (args(0) == "learn") { learn() }
  else throw new UnsupportedOperationException(s"Unrecognized command ${args(0)}")

}
