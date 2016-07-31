package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
      n <- Arbitrary.arbitrary[List[Int]]
    } yield n.foldRight[H](empty)((next: Int, acc: H) => insert(next, acc))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insert an element into an empty heap, delete the minimum, the result should be empty") = forAll {
    (i1: Int) => deleteMin(insert(i1, empty)) == empty
  }

  property("min1") = forAll {
    (i: Int) =>
      findMin(insert(i, empty)) == i
  }

  property("minimum of two-element heap is the minimum of the two") = forAll { (i1: Int, i2: Int) =>
    val h = insert(i1, insert(i2, empty))
    findMin(h) == List(i1, i2).min
  }

  def getMinima(h: H): List[Int] =
    if (isEmpty(h)) List()
    else findMin(h) :: getMinima(deleteMin(h))

  property("getMinima returns a list of the right size") = forAll {
    (i: Int, h: H) =>
      val augmentedHeap = insert(i, h)
      getMinima(augmentedHeap).size == (getMinima(h).size + 1)
  }

  property("getMinima appears to contain everything ") = forAll {
    (i: Int, h: H) =>
      val augmentedHeap = insert(i, h)
      getMinima(augmentedHeap) contains i

  }

  def size(h: H): Int =
    getMinima(h).size

  def stepsToEmpty(h: H): Int = ???

  property("given a heap, successive deleted minima should be ordered") = forAll {
    (h: H) => {
      val minima = getMinima(h)
      minima.sorted == minima
    }
  }

  property("minimum of melded two heaps is the minimum of one or the other") = forAll {
    (h1: H, h2: H) => {
      val melded = meld(h1, h2)
      if (isEmpty(melded)) true
      else if (isEmpty(h1)) findMin(melded) == findMin(h2)
      else if (isEmpty(h2)) findMin(melded) == findMin(h1)
      else {
        val minOfMelded = findMin(melded)
        minOfMelded == findMin(h1) || minOfMelded == findMin(h2)
      }
    }
  }

  property("deleteMin should always produce a smaller heap by 1, unless empty") = forAll {
    (h: H) =>
      if (isEmpty(h)) true
      else size(h) == (size(deleteMin(h)) + 1)
  }

  property("if min -1 is inserted, findMin should still return min - 1") = forAll {
    (h: H) =>
      if (isEmpty(h)) true
      else {
        val min = findMin(h)
        if (min == Int.MinValue) true
        else min -1 == findMin(insert(min - 1, h))
      }
  }

  property("isEmpty works") = forAll {
    (i: Int) => isEmpty(empty)
  }

  property("melding a heap with empty should return the same heap") = forAll {
    (h: H) => meld(h, empty) == h
  }

  property("melding two empties should return empty") = forAll {
    (i: Int) => isEmpty(meld(empty, empty))
  }

  property("calling findMin twice should return the same number") = forAll {
    (h: H) => if (isEmpty(h)) true else findMin(h) == findMin(h)
  }

  property("insert in a melded heap works") = forAll {
    (h1: H, h2: H) =>
      val melded = meld(h1, h2)
      if (isEmpty(melded)) true
      else {
        val min = findMin(melded)
        if (min > Int.MinValue) min - 1 == findMin(insert(min - 1, melded))
        else true
      }
  }

  property("inserting the min should yield a 1-bigger heap") = forAll {
    (h: H) =>
      if (isEmpty(h)) true
      else {
        val originalSize = size(h)
        val newSize = size(insert(findMin(h), h))
        originalSize + 1 == newSize
      }
  }

  property("deleteMin deletes only one min") = forAll {
    (h: H) =>
      if (isEmpty(h)) true
      else {
        val size = getMinima(h).size
        getMinima(deleteMin(h)).size == size - 1
      }
  }

  property("deleteMin is pure") = forAll {
    (h: H) =>
      if (isEmpty(h)) true
      else {
        val altered = deleteMin(h)
        altered != h
      }
  }
  property("insert is pure") = forAll {
    (h: H, i: Int) =>
      val altered = insert(i, h)
      h != altered
  }
  property("meld is pure") = forAll {
    (h1: H, h2: H) =>
      if (isEmpty(h1) || isEmpty(h2)) true
      else {
        meld(h1, h2) != h1 && meld(h1, h2) != h2
      }
  }

  property("findMin should not return the last inserted item") = forAll {
    (h1: H) =>
      if (isEmpty(h1)) true
      else {
        val min = findMin(h1)
        if (min == Int.MaxValue) true
        else findMin(insert(min + 1, h1)) == min
      }
  }

  property("insert always produces a non-empty heap") = forAll {
    (h1: H, i: Int) =>
      !isEmpty(insert(i, h1))
  }

  property("queue can be reconstructed identically by its constituents") = forAll {
    (h1: H) =>
      val minima = getMinima(h1)
      getMinima(minima.foldRight(empty)((i: Int, heap: H) => insert(i, heap))) == minima
  }

  property("the order of arguments in meld should not matter") = forAll {
    (h1: H, h2: H) =>
      if (isEmpty(h1) || isEmpty(h2)) true
      else findMin(meld(h1, h2)) == findMin(meld(h2, h1))
  }

  property("isEmpty is accurate") = forAll {
    (h: H) =>
      if (isEmpty(h)) h == empty
      else getMinima(h).size > 0
  }

  property("empty is effectively empty") = forAll {
    (i: Int) =>
      getMinima(empty).size == 0
  }

  property("deleteMin returns a working heap") = forAll {
    (h: H) =>
      if (isEmpty(h)) true
      else {
        val altered = deleteMin(h)
        getMinima(altered) == getMinima(h).tail
      }
  }

  property("removing, then adding the same element should produce the same heap") = forAll {
    (h: H) =>
      if (isEmpty(h)) true
      else {
        getMinima(insert(findMin(h), deleteMin(h))) == getMinima(h)
      }
  }

  property("inserting in the middle of the heap should produce a working heap") = forAll {
    (h: H) =>
      val minima = getMinima(h)
      val roughMiddle = minima.size / 2
      if (minima.size < 2) true
      else {
        val quasiMedian = minima(roughMiddle)
        val modifiedHeap = insert(quasiMedian + 1, h)
        val modifiedMinima = getMinima(modifiedHeap)
        modifiedMinima == modifiedMinima.sorted
      }
  }
}
