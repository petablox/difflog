package qd
package values

import org.scalatest.{FunSuite, Ignore}
import org.scalacheck.Prop.forAll


class DValueTest extends FunSuite {
  test("Testing") {
    val d1 = DValue(Set(Set(1,2)))
    val d2 = DValue(Set(Set(1), Set(2)))
    val union1 = d1 + d2
    assert(union1.r contains Set(1))
    assert(union1.r contains Set(2))
    println(union1)

    val d3 = DValue(Set(Set(1,2), Set(1,3)))
    val union2 = d2 + d3
    println(union2)

    val product1 = d1 * d3
    println(product1)

    val d4 = DValue(Set(Set(1,2,3)))
    val union3 = d1 + d4
    println(union3)

    val o1 = DValue(Set(Set(1, 5), Set(1, 14)))
    val o2 = DValue(Set(Set(1, 5, 14)))
    println(o1 < o2)

    val o3 = DValue(Set(Set(1,5)))
    val o4 = DValue(Set(Set(1,5,14)))
    println(o3 < o4)
    println(o4 < o3)

    val p1 = DValue(Set(Set(1,2)))
    val p2 = DValue(Set(Set(1)))
    println(p1 * p2)
  }
  //val SetGen : Gen[DValue] = DValue(Arbitrary.arbSet[Set[Int]])


  ignore("Scalacheck") {
    val checkPlusAssoc = forAll { (s1 : Set[Set[Int]], s2 : Set[Set[Int]], s3 : Set[Set[Int]]) =>
      (DValue(s1) + DValue(s2)) + DValue(s3) == DValue(s1) + (DValue(s2) + DValue(s3))}
    checkPlusAssoc.check

    val checkPlusComm = forAll { (s1: Set[Set[Int]], s2 : Set[Set[Int]]) => DValue(s1) + DValue(s2) == DValue(s2) + DValue(s1) }
    checkPlusComm.check

    val checkZeroLeft = forAll { s1 : Set[Set[Int]] => (DValue(s1) + DValue.Zero == DValue(s1).canonize()) }
    checkZeroLeft.check

    val checkZeroRight = forAll { s1 : Set[Set[Int]] => (DValue.Zero + DValue(s1) == DValue(s1).canonize())  }
    checkZeroRight.check

    // Expensive test and probably correct
    //val checkMultAssoc = forAll { (s1 : Set[Set[Int]], s2 : Set[Set[Int]], s3 : Set[Set[Int]]) =>
    //  (DValue(s1) * DValue(s2)) * DValue(s3) == DValue(s1) * (DValue(s2) * DValue(s3))}
    //checkMultAssoc.check

    //val checkMultComm = forAll { (s1: Set[Set[Int]], s2 : Set[Set[Int]]) => DValue(s1) * DValue(s2) == DValue(s2) * DValue(s1) }
    //checkMultComm.check

    val checkOneLeft = forAll { s1 : Set[Set[Int]] => (DValue(s1) * DValue.One == DValue(s1).canonize()) }
    checkOneLeft.check

    val checkOneRight = forAll { s1 : Set[Set[Int]] => (DValue.One * DValue(s1) == DValue(s1).canonize())  }
    checkOneRight.check

    val checkDistr = forAll { (s1: Set[Set[Int]], s2 : Set[Set[Int]], s3: Set[Set[Int]]) =>
      DValue(s3) * (DValue(s1) + DValue(s2)) == DValue(s3) * DValue(s1) + DValue(s3) * DValue(s2) }
    checkDistr.check
  }

  test("Scalacheck - Ordering") {
    val checkPlusCanon = forAll { (s1: Set[Set[Int]], s2: Set[Set[Int]]) =>
      DValue(s1) + DValue(s2) == (DValue(s1) + DValue(s2)).canonize()
    }
    checkPlusCanon.check
  }
}
