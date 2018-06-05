package qd
package learner
import org.scalatest.{FunSuite, Ignore}

class InflammationSoup50 extends Problem {
	override val name = "inflammation"
	val PSet = Range(0, 120).map(i => Atom(i)).toSet
	val P = Domain("P", PSet)
	val nausea = Relation("nausea", P)
	val not_nausea = Relation("not_nausea", P)
	val lumbar_pain = Relation("lumbar_pain", P)
	val not_lumbar_pain = Relation("not_lumbar_pain", P)
	val urine_pushing = Relation("urine_pushing", P)
	val not_urine_pushing = Relation("not_urine_pushing", P)
	val micturition_pain = Relation("micturition_pain", P)
	val not_micturition_pain = Relation("not_micturition_pain", P)
	val burning_urethra = Relation("burning_urethra", P)
	val not_burning_urethra = Relation("not_burning_urethra", P)
	val inflamation = Relation("inflamation", P)
	val not_micturition_painTuples = Set(0,2,4,5,7,11,12,13,14,15,16,19,22,27,28,30,32,33,34,36,37,38,40,41,43,44,45,47,49,50,51,52,54,56,57,59,60,61,62,63,64,65,66,67,68,69,73,74,77,80,82,86,87,90,93,94,95,96,97,100,102,103,104,107,109,111,112,113,115,116,117,118,119).map { case (x0) => DTuple(Atom(x0)) }
	val inflamationTuples = Set(1,3,6,8,9,10,17,18,20,21,23,24,25,26,27,29,30,31,35,36,38,39,42,43,44,46,48,53,55,58,59,70,71,72,78,79,83,84,85,88,89,92,98,99,101,105,106,110,114).map { case (x0) => DTuple(Atom(x0)) }
	val lumbar_painTuples = Set(0,2,5,7,11,14,15,19,22,32,34,37,40,41,50,52,57,60,62,63,64,65,66,67,68,69,70,71,72,75,76,77,78,79,81,82,83,84,85,88,89,91,92,97,98,99,100,101,104,105,106,108,109,110,113,114,115,118).map { case (x0) => DTuple(Atom(x0)) }
	val burning_urethraTuples = Set(1,3,6,8,9,10,17,18,23,24,25,26,29,35,42,53,58,60,62,63,64,65,66,67,68,69,70,71,77,79,82,83,89,92,97,100,101,104,105,109,110,113,115,118,119).map { case (x0) => DTuple(Atom(x0)) }
	val not_urine_pushingTuples = Set(0,2,5,7,11,14,15,19,22,32,34,37,40,41,50,52,57,73,74,75,76,80,81,86,90,91,94,102,107,108,111,116).map { case (x0) => DTuple(Atom(x0)) }
	val not_burning_urethraTuples = Set(0,2,4,5,7,11,12,13,14,15,16,19,20,21,22,27,28,30,31,32,33,34,36,37,38,39,40,41,43,44,45,46,47,48,49,50,51,52,54,55,56,57,59,61,72,73,74,75,76,78,80,81,84,85,86,87,88,90,91,93,94,95,96,98,99,102,103,106,107,108,111,112,114,116,117).map { case (x0) => DTuple(Atom(x0)) }
	val urine_pushingTuples = Set(1,3,4,6,8,9,10,12,13,16,17,18,20,21,23,24,25,26,27,28,29,30,31,33,35,36,38,39,42,43,44,45,46,47,48,49,51,53,54,55,56,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,77,78,79,82,83,84,85,87,88,89,92,93,95,96,97,98,99,100,101,103,104,105,106,109,110,112,113,114,115,117,118,119).map { case (x0) => DTuple(Atom(x0)) }
	val not_lumbar_painTuples = Set(1,3,4,6,8,9,10,12,13,16,17,18,20,21,23,24,25,26,27,28,29,30,31,33,35,36,38,39,42,43,44,45,46,47,48,49,51,53,54,55,56,58,59,61,73,74,80,86,87,90,93,94,95,96,102,103,107,111,112,116,117,119).map { case (x0) => DTuple(Atom(x0)) }
	val not_nauseaTuples = Set(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,73,74,77,80,82,86,87,90,93,94,95,96,97,100,102,103,104,107,109,111,112,113,115,116,117,118,119).map { case (x0) => DTuple(Atom(x0)) }
	val micturition_painTuples = Set(1,3,6,8,9,10,17,18,20,21,23,24,25,26,29,31,35,39,42,46,48,53,55,58,70,71,72,75,76,78,79,81,83,84,85,88,89,91,92,98,99,101,105,106,108,110,114).map { case (x0) => DTuple(Atom(x0)) }
	val nauseaTuples = Set(70,71,72,75,76,78,79,81,83,84,85,88,89,91,92,98,99,101,105,106,108,110,114).map { case (x0) => DTuple(Atom(x0)) }
  override val edb = Config(
    nausea -> (Instance[FValue](nausea) ++ nauseaTuples.map(t => t -> FValue.One).toMap),
    not_nausea -> (Instance[FValue](not_nausea) ++ not_nauseaTuples.map(t => t -> FValue.One).toMap),
    lumbar_pain -> (Instance[FValue](lumbar_pain) ++ lumbar_painTuples.map(t => t -> FValue.One).toMap),
    not_lumbar_pain -> (Instance[FValue](not_lumbar_pain) ++ not_lumbar_painTuples.map(t => t -> FValue.One).toMap),
    urine_pushing -> (Instance[FValue](urine_pushing) ++ urine_pushingTuples.map(t => t -> FValue.One).toMap),
    not_urine_pushing -> (Instance[FValue](not_urine_pushing) ++ not_urine_pushingTuples.map(t => t -> FValue.One).toMap),
    micturition_pain -> (Instance[FValue](micturition_pain) ++ micturition_painTuples.map(t => t -> FValue.One).toMap),
    not_micturition_pain -> (Instance[FValue](not_micturition_pain) ++ not_micturition_painTuples.map(t => t -> FValue.One).toMap),
    burning_urethra -> (Instance[FValue](burning_urethra) ++ burning_urethraTuples.map(t => t -> FValue.One).toMap),
    not_burning_urethra -> (Instance[FValue](not_burning_urethra) ++ not_burning_urethraTuples.map(t => t -> FValue.One).toMap),
    )
  override val refOut = Config(
    inflamation -> (Instance[FValue](inflamation) ++ inflamationTuples.map(t => t -> FValue.One).toMap),
    )
	val x0P = Variable("x0P",P)
	val soup = Set(
		Rule(0, FValue(0.5, Token(0)), inflamation(x0P),lumbar_pain(x0P),not_burning_urethra(x0P)),
		Rule(1, FValue(0.5, Token(1)), inflamation(x0P),lumbar_pain(x0P),not_micturition_pain(x0P)),
		Rule(2, FValue(0.5, Token(2)), inflamation(x0P),lumbar_pain(x0P),urine_pushing(x0P)),
		Rule(3, FValue(0.5, Token(3)), inflamation(x0P),lumbar_pain(x0P),micturition_pain(x0P)),
		Rule(4, FValue(0.5, Token(4)), inflamation(x0P),burning_urethra(x0P),lumbar_pain(x0P)),
		Rule(10, FValue(0.5, Token(10)), inflamation(x0P),not_burning_urethra(x0P),urine_pushing(x0P)),
		Rule(11, FValue(0.5, Token(11)), inflamation(x0P),micturition_pain(x0P),not_burning_urethra(x0P)),
		Rule(17, FValue(0.5, Token(17)), inflamation(x0P),not_burning_urethra(x0P),not_micturition_pain(x0P)),
		Rule(18, FValue(0.5, Token(18)), inflamation(x0P),not_micturition_pain(x0P),urine_pushing(x0P)),
		Rule(19, FValue(0.5, Token(19)), inflamation(x0P),micturition_pain(x0P),not_micturition_pain(x0P)),
		Rule(20, FValue(0.5, Token(20)), inflamation(x0P),burning_urethra(x0P),not_micturition_pain(x0P)),
		Rule(21, FValue(0.5, Token(21)), inflamation(x0P),not_micturition_pain(x0P),not_nausea(x0P)),
		Rule(22, FValue(0.5, Token(22)), inflamation(x0P),not_lumbar_pain(x0P),not_micturition_pain(x0P)),
		Rule(23, FValue(0.5, Token(23)), inflamation(x0P),not_micturition_pain(x0P),not_urine_pushing(x0P)),
		Rule(24, FValue(0.5, Token(24)), inflamation(x0P),nausea(x0P),not_micturition_pain(x0P)),
		Rule(25, FValue(0.5, Token(25)), inflamation(x0P),not_burning_urethra(x0P),urine_pushing(x0P)),
		Rule(26, FValue(0.5, Token(26)), inflamation(x0P),not_micturition_pain(x0P),urine_pushing(x0P)),
		Rule(30, FValue(0.5, Token(30)), inflamation(x0P),not_lumbar_pain(x0P),urine_pushing(x0P)),
		Rule(31, FValue(0.5, Token(31)), inflamation(x0P),not_urine_pushing(x0P),urine_pushing(x0P)),
		Rule(32, FValue(0.5, Token(32)), inflamation(x0P),nausea(x0P),urine_pushing(x0P)),
		Rule(37, FValue(0.5, Token(37)), inflamation(x0P),micturition_pain(x0P),not_nausea(x0P)),
		Rule(38, FValue(0.5, Token(38)), inflamation(x0P),micturition_pain(x0P),not_lumbar_pain(x0P)),
		Rule(39, FValue(0.5, Token(39)), inflamation(x0P),micturition_pain(x0P),not_urine_pushing(x0P)),
		Rule(40, FValue(0.5, Token(40)), inflamation(x0P),micturition_pain(x0P),nausea(x0P)),
		Rule(41, FValue(0.5, Token(41)), inflamation(x0P),burning_urethra(x0P),not_burning_urethra(x0P)),
		Rule(49, FValue(0.5, Token(49)), inflamation(x0P),not_burning_urethra(x0P),not_nausea(x0P)),
		Rule(50, FValue(0.5, Token(50)), inflamation(x0P),not_micturition_pain(x0P),not_nausea(x0P)),
		Rule(51, FValue(0.5, Token(51)), inflamation(x0P),not_nausea(x0P),urine_pushing(x0P)),
		Rule(52, FValue(0.5, Token(52)), inflamation(x0P),micturition_pain(x0P),not_nausea(x0P)),
		Rule(53, FValue(0.5, Token(53)), inflamation(x0P),burning_urethra(x0P),not_nausea(x0P)),
		Rule(54, FValue(0.5, Token(54)), inflamation(x0P),not_lumbar_pain(x0P),not_nausea(x0P)),
    Rule(71, FValue(0.5, Token(71)), inflamation(x0P),not_lumbar_pain(x0P),not_urine_pushing(x0P)),
		Rule(72, FValue(0.5, Token(72)), inflamation(x0P),nausea(x0P),not_urine_pushing(x0P)),
		Rule(73, FValue(0.5, Token(73)), inflamation(x0P),nausea(x0P),not_burning_urethra(x0P)),
		Rule(75, FValue(0.5, Token(75)), inflamation(x0P),nausea(x0P),urine_pushing(x0P)),
		Rule(76, FValue(0.5, Token(76)), inflamation(x0P),micturition_pain(x0P),nausea(x0P)),
		Rule(77, FValue(0.5, Token(77)), inflamation(x0P),burning_urethra(x0P),nausea(x0P)),
		Rule(78, FValue(0.5, Token(78)), inflamation(x0P),nausea(x0P),not_nausea(x0P)),
		Rule(79, FValue(0.5, Token(79)), inflamation(x0P),nausea(x0P),not_lumbar_pain(x0P)),
		Rule(80, FValue(0.5, Token(80)), inflamation(x0P),nausea(x0P),not_urine_pushing(x0P)),
	)

	override val expected = Set(30,75)
	override val maxVarCount: Int = 20
}
