package qd
package learner

import org.scalatest.Ignore

@Ignore
class Modref extends Problem {
  override val name: String = "Modref"

  val mSet : Set[Atom] = Range(0, 8).map(i => Atom(i)).toSet
  val m : Domain = Domain("Method", mSet)

  val vSet : Set[Atom] = Range(0, 8).map(i => Atom(i)).toSet
  val v : Domain = Domain("Variable", vSet)

  val hSet : Set[Atom] = Range(0, 8).map(i => Atom(i)).toSet
  val h : Domain = Domain("Heap", hSet)

  val fSet : Set[Atom] = Range(0, 8).map(i => Atom(i)).toSet
  val f : Domain = Domain("F", fSet)

  val iSet : Set[Atom] = Range(0, 8).map(i => Atom(i)).toSet
  val i : Domain = Domain("I", iSet)

  val MgetInstFldInst : Relation = Relation("MgetInstFldInst", m, v, v, f)
  val MputInstFldInst : Relation = Relation("MputInstFldInst", m, v, f, v)
  val MgetStatFldInst : Relation = Relation("MgetStatFldInst", m, v, f)
  val MputStatFldInst : Relation = Relation("MputStatFldInst", m, f, v)
  val VH : Relation = Relation("VH", v, h)
  val IM : Relation = Relation("IM", i, m)
  val MI : Relation = Relation("MI", m, i)

  val rMM : Relation = Relation("rMM", m, m)
  val refStatField : Relation = Relation("refStatField", m, f)
  val modStatField : Relation = Relation("modStatField", m, f)
  val refInstField : Relation = Relation("refInstField", m, h, f)
  val modInstField : Relation = Relation("modInstField", m, h, f)

  val MgetInstFldInstTuples : Set[DTuple] = Set((4,3,2,2)).map{ case (a,b,c,d) => DTuple(Atom(a), Atom(b), Atom(c), Atom(d)) }
  val MputInstFldInstTuples : Set[DTuple] = Set((4,4,3,5)).map{ case (a,b,c,d) => DTuple(Atom(a), Atom(b), Atom(c), Atom(d)) }
  val MgetStatFldInstTuples : Set[DTuple] = Set((4,0,0),(5,6,4),(7,6,6)).map{ case (a,b,c) => DTuple(Atom(a), Atom(b), Atom(c))}
  val MputStatFldInstTuples : Set[DTuple] = Set((4,1,1),(6,5,7),(7,6,7)).map{ case (a,b,c) => DTuple(Atom(a), Atom(b), Atom(c))}
  val VHTuples : Set[DTuple] = Set((2,2),(4,4)).map{ case (a,b) => DTuple(Atom(a), Atom(b)) }
  val MITuples : Set[DTuple] = Set((0,0), (1,1), (2,2), (3,3)).map{ case (a,b) => DTuple(Atom(a), Atom(b)) }
  val IMTuples : Set[DTuple] = Set((0,1), (1,2), (2,3), (3,4)).map{ case (a,b) => DTuple(Atom(a), Atom(b)) }

  override val edb : Config = Config(
    MgetInstFldInst -> (Instance(MgetInstFldInst) ++ MgetInstFldInstTuples.map(t => t -> One).toMap),
    MputInstFldInst -> (Instance(MputInstFldInst) ++ MputInstFldInstTuples.map(t => t -> One).toMap),
    MgetStatFldInst -> (Instance(MgetStatFldInst) ++ MgetStatFldInstTuples.map(t => t -> One).toMap),
    MputStatFldInst -> (Instance(MputStatFldInst) ++ MputStatFldInstTuples.map(t => t -> One).toMap),
    VH -> (Instance(VH) ++ VHTuples.map(t => t -> One).toMap),
    IM -> (Instance(IM) ++ IMTuples.map(t => t -> One).toMap),
    MI -> (Instance(MI) ++ MITuples.map(t => t -> One).toMap),
  )

  val rMMTuples : Set[DTuple] = Set((0,1),(0,2),(0,3),(0,4),(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)).map{ case (a,b) => DTuple(Atom(a), Atom(b)) }
  val refStatFieldTuples : Set[DTuple] = Set((0,0),(1,0),(2,0),(3,0),(4,0),(5,4),(7,6)).map{ case (a,b) => DTuple(Atom(a), Atom(b)) }
  val modStatFieldTuples : Set[DTuple] = Set((0,1),(1,1),(2,1),(3,1),(4,1),(6,5),(7,6)).map{ case (a,b) => DTuple(Atom(a), Atom(b)) }
  val refInstFieldTuples : Set[DTuple] = Set((0,2,2),(1,2,2),(2,2,2),(3,2,2),(4,2,2)).map{ case (a,b,c) => DTuple(Atom(a), Atom(b), Atom(c)) }
  val modInstFieldTuples : Set[DTuple] = Set((0,4,3),(1,4,3),(2,4,3),(3,4,3),(4,4,3)).map{ case (a,b,c) => DTuple(Atom(a), Atom(b), Atom(c))}

  override val refOut : Config = Config (
    rMM -> (Instance(rMM) ++ rMMTuples.map(t => t -> One).toMap),
    refStatField -> (Instance(refStatField) ++ refStatFieldTuples.map(t => t -> One).toMap),
    modStatField -> (Instance(modStatField) ++ modStatFieldTuples.map(t => t -> One).toMap),
    refInstField -> (Instance(refInstField) ++ refInstFieldTuples.map(t => t -> One).toMap),
    modInstField -> (Instance(modInstField) ++ modInstFieldTuples.map(t => t -> One).toMap),
  )

  val x0H : Variable = Variable("x0H", h)
  val x1H : Variable = Variable("x1H", h)
  val x2H : Variable = Variable("x2H", h)
  val x3H : Variable = Variable("x3H", h)

  val x0V : Variable = Variable("x0V", v)
  val x1V : Variable = Variable("x1V", v)
  val x2V : Variable = Variable("x2V", v)
  val x3V : Variable = Variable("x3V", v)
  val x4V : Variable = Variable("x4V", v)

  val x0M : Variable = Variable("x0M", m)
  val x1M : Variable = Variable("x1M", m)
  val x2M : Variable = Variable("x2M", m)
  val x3M : Variable = Variable("x3M", m)

  val x0I : Variable = Variable("x0I", i)
  val x1I : Variable = Variable("x1I", i)
  val x2I : Variable = Variable("x2I", i)
  val x3I : Variable = Variable("x3I", i)

  val x0F : Variable = Variable("x0F", f)
  val x1F : Variable = Variable("x1F", f)
  val x2F : Variable = Variable("x2F", f)
  val x3F : Variable = Variable("x3F", f)

  // expected: 1, 2, 9, 13, 17, 22, 25, 26, 27, 29
  override val soup: Set[Rule] = Set(
    Rule(1	,Value(0.5, Token(1	)),rMM(x0M,x1M), rMM(x0M,x2M),rMM(x2M,x1M)),
    Rule(2	,Value(0.5, Token(2	)),rMM(x0M,x1M), IM(x2I,x1M),MI(x0M,x2I)),
    Rule(3	,Value(0.5, Token(3	)),rMM(x0M,x1M), rMM(x2M,x0M),rMM(x2M,x3M),rMM(x3M,x1M)),
    Rule(4	,Value(0.5, Token(4	)),rMM(x0M,x1M), rMM(x0M,x2M),rMM(x2M,x3M),rMM(x3M,x1M)),
    Rule(5	,Value(0.5, Token(5	)),rMM(x0M,x1M), rMM(x0M,x2M),rMM(x1M,x2M)),
    Rule(6	,Value(0.5, Token(6	)),rMM(x0M,x1M), refStatField(x0M,x2F),refStatField(x1M,x2F)),
    Rule(7	,Value(0.5, Token(7	)),rMM(x0M,x1M), modStatField(x0M,x2F),modStatField(x1M,x2F)),
    Rule(8	,Value(0.5, Token(8	)),rMM(x0M,x1M), MI(x0M,x2I),MI(x1M,x2I)),
    Rule(9	,Value(0.5, Token(9	)),refStatField(x0M,x1F), rMM(x0M,x2M),refStatField(x2M,x1F)),
    Rule(10	,Value(0.5, Token(10	)),refStatField(x0M,x1F), modStatField(x2M,x1F),rMM(x0M,x2M)),
    Rule(11	,Value(0.5, Token(11	)),refStatField(x0M,x1F), refInstField(x0M,x2H,x1F)),
    Rule(12	,Value(0.5, Token(12	)),refStatField(x0M,x1F), modInstField(x0M,x2H,x1F)),
    Rule(13	,Value(0.5, Token(13	)),refStatField(x0M,x1F), MgetStatFldInst(x0M,x2V,x1F)),
    Rule(14	,Value(0.5, Token(14	)),refStatField(x0M,x1F), modStatField(x0M,x1F)),
    Rule(15	,Value(0.5, Token(15	)),refStatField(x0M,x1F), MputStatFldInst(x0M,x1F,x2V)),
    Rule(16	,Value(0.5, Token(16	)),modStatField(x0M,x1F), rMM(x0M,x2M),refStatField(x2M,x1F)),
    Rule(17	,Value(0.5, Token(17	)),modStatField(x0M,x1F), modStatField(x2M,x1F),rMM(x0M,x2M)),
    Rule(18	,Value(0.5, Token(18	)),modStatField(x0M,x1F), refInstField(x0M,x2H,x1F)),
    Rule(19	,Value(0.5, Token(19	)),modStatField(x0M,x1F), modInstField(x0M,x2H,x1F)),
    Rule(20	,Value(0.5, Token(20	)),modStatField(x0M,x1F), MgetStatFldInst(x0M,x2V,x1F)),
    Rule(21	,Value(0.5, Token(21	)),modStatField(x0M,x1F), refStatField(x0M,x1F)),
    Rule(22	,Value(0.5, Token(22	)),modStatField(x0M,x1F), MputStatFldInst(x0M,x1F,x2V)),
    Rule(23	,Value(0.5, Token(23	)),refInstField(x0M,x1H,x2F), MputInstFldInst(x0M,x3V,x2F,x4V),VH(x3V,x1H)),
    Rule(24	,Value(0.5, Token(24	)),refInstField(x0M,x1H,x2F), modInstField(x0M,x1H,x2F)),
    Rule(25	,Value(0.5, Token(25	)),refInstField(x0M,x1H,x2F), rMM(x0M,x3M),refInstField(x3M,x1H,x2F)),
    Rule(26	,Value(0.5, Token(26	)),refInstField(x0M,x1H,x2F), MgetInstFldInst(x0M,x3V,x4V,x2F),VH(x4V,x1H)),
    Rule(27	,Value(0.5, Token(27	)),modInstField(x0M,x1H,x2F), MputInstFldInst(x0M,x3V,x2F,x4V),VH(x3V,x1H)),
    Rule(28	,Value(0.5, Token(28	)),modInstField(x0M,x1H,x2F), refInstField(x0M,x1H,x2F)),
    Rule(29	,Value(0.5, Token(29	)),modInstField(x0M,x1H,x2F), modInstField(x3M,x1H,x2F),rMM(x0M,x3M)),
    Rule(30	,Value(0.5, Token(30	)),modInstField(x0M,x1H,x2F), MgetInstFldInst(x0M,x3V,x4V,x2F),VH(x4V,x1H)),
  )

  override val expected: Set[Any] = Set(1, 2, 9, 13, 15, 17, 25, 26, 27, 29)
  override val maxVarCount: Int = 20
}