package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times in a simple list") {
    assert(times(string2Chars("carrotcake"))
      === List(('c', 2), ('a', 2), ('r', 2), ('o', 1), ('t', 1), ('k', 1), ('e', 1)))
  }

  test("singleton") {
    new TestTrees {
      assert(singleton(List(t1)) === true)
    }
  }

  test("not singleton") {
    new TestTrees {
      assert(singleton(List(t1, t2)) === false)
    }
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("test decoding") {
    assert(decodedSecret.mkString === "huffmanestcool")
  }
  
  test("createCodeTree gives the expected output on small phrase") {
    assert(createCodeTree(List('c', 'o', 'u', 'c', 'o', 'u')) === Fork(Fork(Leaf('c', 2), Leaf('o', 2), List('c', 'o'), 4), Leaf('u', 2), List('c', 'o', 'u'), 6))
  }

  test("makeOrderedLeafList works") {
    assert(makeOrderedLeafList(List(('a', 5), ('b', 12), ('c', 2), ('e', 44)))
      === List(Leaf('c', 2), Leaf('a', 5), Leaf('b', 12), Leaf('e', 44)))
  }

  test("createCodeTree’s total weight is right") {
    val example = List('c', 'a', 'r', 'r', 'o', 't', 'c', 'a', 'k', 'e')
    assert(weight(createCodeTree(example)) === times(example).fold((null, 0))((a, b) => (null, a._2 + b._2))._2)
  }

  test("explore what happens with long strings") {
    val longText = string2Chars("Outside Haskell, you either target the backends directly or use something like ArrayFire, but my impression is that nothing provides a nicer overall abstraction with high-level and yet perfomant language, while being embedded in an aesthetically pleasing (subjective) language. That said, I may be unaware of something better.")
    assert(makeOrderedLeafList(times(longText)) !== null)
  }
}
