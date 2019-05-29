package qd
package problem

import java.io.{File, PrintWriter}

object SouffleWriter {

  def write(query: Problem, outputDirectoryName: String): Unit = {
    val outputDirectory = new File(outputDirectoryName)
    if (!outputDirectory.exists()) {
      outputDirectory.mkdir()
    }
    require(outputDirectory.isDirectory)

    writeRules(query, outputDirectoryName)
    writeInputRels(query, outputDirectoryName)
    writeOutputRels(query, outputDirectoryName)
  }

  def writeRules(query: Problem, outputDirectoryName: String): Unit = {
    val rulesFile = new PrintWriter(outputDirectoryName + "/rules.dl")
    val ruleNamesFile = new PrintWriter(outputDirectoryName + "/ruleNames.txt")

    try {

      val allDomains = (query.inputRels ++ query.inventedRels ++ query.outputRels).flatMap(_.signature)
      allDomains.foreach(domain => rulesFile.println(s".type $domain"))
      rulesFile.println("")

      rulesFile.println(".decl Rule(n: number)")
      rulesFile.println(".input Rule")
      rulesFile.println("")

      query.inputRels.foreach { rel =>
        val indexedSignature = rel.signature.zipWithIndex.map({ case (domain, index) => s"v$index: $domain" })
        rulesFile.println(s".decl ${rel.name}(${indexedSignature.mkString(", ")})")
        rulesFile.println(s".input ${rel.name}")
      }
      rulesFile.println("")

      (query.inventedRels ++ query.outputRels).foreach { rel =>
        val indexedSignature = rel.signature.zipWithIndex.map({ case (domain, index) => s"v$index: $domain" })
        rulesFile.println(s".decl ${rel.name}(${indexedSignature.mkString(", ")})")
        rulesFile.println(s".output ${rel.name}")
      }
      rulesFile.println("")

      query.rules.zipWithIndex.foreach { case (rule, index) =>
        rulesFile.println(s"${rule.head} :- ${rule.body.mkString(", ")}, Rule($index).")
        ruleNamesFile.println(index)
      }

    } finally {
      rulesFile.close()
      ruleNamesFile.close()
    }
  }

  def writeInputRels(query: Problem, outputDirectoryName: String): Unit = {
    for (rel <- query.inputRels) {
      val relFile = new PrintWriter(outputDirectoryName + s"/${rel.name}.facts")
      try {
        query.discreteEDB(rel).foreach { t =>
          relFile.println(t.fields.mkString("\t"))
        }
      } finally {
        relFile.close()
      }
    }
  }

  def writeOutputRels(query: Problem, outputDirectoryName: String): Unit = {
    for (rel <- query.outputRels) {
      val relFile = new PrintWriter(outputDirectoryName + s"/${rel.name}.expected")
      try {
        query.discreteIDB(rel).foreach { t =>
          relFile.println(t.fields.mkString("\t"))
        }
      } finally {
        relFile.close()
      }
    }
  }

}
