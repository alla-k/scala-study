package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =
    oneOf(const(empty),
      for {
        k <- arbitrary[Int]
        m <- oneOf(const(empty), genHeap)
      } yield insert(k, m)
    )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("minOfTwo") = forAll { (a: Int,  b:Int) =>
    (a > b) ==> {
      val h = insert(a, empty)
      val h2 = insert(b, h)
      findMin(h2) == b
    }
  }

  property("minEmpty") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h)
    isEmpty(h)
  }

  property("minOfTwoHeaps") = forAll { (h: H, h1:H) =>
    val m1 = if (isEmpty(h)) 0 else findMin(h)
    val m2 = if (isEmpty(h1)) 0 else findMin(h1)
    val hMin = meld(h, h1)
    if(!isEmpty(hMin)) { findMin(hMin) == m1 || findMin(hMin) == m2 } else true
  }

  property("sortedSeq") = forAll { (h: H) =>
    def makeSeq(h:H):List[Int] = {
      val m = if (isEmpty(h)) 0 else findMin(h)
      val h2 = deleteMin(h)
      m :: makeSeq(h2)
    }
    makeSeq(h).sorted == makeSeq(h)
  }

}
