package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math.min

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency((1, Gen.const(empty)), (9, genHeap))
  } yield insert(n, h)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: A =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /**
    * If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the
    * smallest of the two elements back.
    */
  property("prop1") = forAll { (a1: A, a2: A) =>
    val h = insert(a1, insert(a2, empty))
    findMin(h) == {if (a1 < a2) a1 else a2}
  }

  /**
    * If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
    */
  property("prop2") = forAll { a: A =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  /**
    * Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
    * (Hint: recursion and helper functions are your friends.)
    */
  property("prop3") = forAll { h: H =>
    def getElems(h: H): Stream[A] = h match {
      case h if isEmpty(h) => Stream()
      case h => findMin(h) #::  getElems(deleteMin(h))
    }
    val l = getElems(h)
    l == l.sorted
  }

  /**
    * Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
    */
  property("prop4") = forAll { (h1: H, h2: H) =>
    val m = meld(h1, h2)
    (isEmpty(h1), isEmpty(h2)) match {
      case (true, true) => throws(classOf[NoSuchElementException]) (findMin(h1))
      case (true, false) => findMin(m) == findMin(h2)
      case (false, true) => findMin(m) == findMin(h1)
      case (false, false) => (findMin(m) == findMin(h1)) || (findMin(m) == findMin(h2))
    }
  }
  /**
    * Two heaps should be equal if recursivly removing min elements result in same elements until empty
    */
  property("prop5") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    heapEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

}
