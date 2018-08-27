package qd

import org.scalatest.FunSuite
import qd.Semiring.FValueSemiringObj

class InstanceSpec extends FunSuite {

  val alphabet = Domain(classOf[Character])
  val digit = Domain(classOf[Int])

  val a = Constant('a', alphabet)
  val b = Constant('b', alphabet)
  val c = Constant('c', alphabet)

  val d0 = Constant(0, digit)
  val d1 = Constant(1, digit)
  val d2 = Constant(2, digit)

  val ad0 = DTuple(a, d0)
  val ad1 = DTuple(a, d1)
  val abd0 = DTuple(a, b, d0)
  val cbd0 = DTuple(c, b, d0)

  val f1 = FValue(0.3, Empty)
  val f2 = FValue(0.25, Empty)
  val f12: FValue = f1 + f2

  val instAD1: Instance[FValue] = Instance(alphabet, digit)
  val instAD2: Instance[FValue] = instAD1 + (ad0 -> f1)

  val instAAD1: Instance[FValue] = Instance(alphabet, alphabet, digit)
  val instAAD2: Instance[FValue] = instAAD1 + (abd0 -> f2)
  val instAAD3: Instance[FValue] = instAAD2 + (abd0 -> f1)
  val instAAD4: Instance[FValue] = instAAD2 + (cbd0 -> f12)

  test("Signatures are computed correctly") {
    assert(instAD1.signature == Seq(alphabet, digit))
    assert(instAD2.signature == Seq(alphabet, digit))

    assert(instAAD1.signature == Seq(alphabet, alphabet, digit))
    assert(instAAD2.signature == Seq(alphabet, alphabet, digit))
    assert(instAAD3.signature == Seq(alphabet, alphabet, digit))
    assert(instAAD4.signature == Seq(alphabet, alphabet, digit))
  }

  test("Supports are computed correctly") {
    assert(instAD1.support == Set())
    assert(instAD2.support == Set((ad0, f1)))

    assert(instAAD1.support == Set())
    assert(instAAD2.support == Set((abd0, f2)))
    assert(instAAD3.support == Set((abd0, f12)))
    assert(instAAD4.support == Set((abd0, f2), (cbd0, f12)))
  }

  test("Values are retrieved correctly") {
    assert(instAD1(ad0) == FValueSemiringObj.Zero)
    assert(instAD1(ad1) == FValueSemiringObj.Zero)

    assert(instAD2(ad0) == f1)
    assert(instAD2(ad1) == FValueSemiringObj.Zero)
  }

  test("Instances are filtered correctly") {
    def fmatch(f: Seq[Option[Constant]], t: Seq[Constant]): Boolean = {
      require(f.length == t.length, s"$f, $t")
      if (f.nonEmpty) {
        val fhead = f.head
        val thead = t.head
        if (fhead.nonEmpty) fhead.get == thead && fmatch(f.tail, t.tail)
        else fmatch(f.tail, t.tail)
      } else true
    }

    for (inst <- Set(instAD1, instAD2);
         f <- Set(Seq(None, None),
                  Seq(Some(a), None),
                  Seq(Some(b), None),
                  Seq(Some(a), Some(b)),
                  Seq(None, Some(c)))) {
      assert(inst.filter(f) == inst.support.filter { case (tuple, _) => fmatch(f, tuple) })
    }

    for (inst <- Set(instAAD1, instAAD2, instAAD3, instAAD4);
         f <- Set(Seq(None, None, None),
                  Seq(Some(a), None, Some(d0)),
                  Seq(Some(b), Some(c), Some(d1)),
                  Seq(Some(a), Some(b), None),
                  Seq(None, Some(c), Some(d2)))) {
      assert(inst.filter(f) == inst.support.filter { case (tuple, _) => fmatch(f, tuple) })
    }
  }

  test("Tuples are correctly added to instances") {
    assert(instAD1 + (ad1 -> f2) + (ad0 -> f1) == instAD2 + (ad1 -> f2))
    assert(instAD2 + (ad0 -> f1) == instAD2)
    assert(instAD1 + (ad0 -> f2) + (ad0 -> f1) == instAD2)
  }

}
