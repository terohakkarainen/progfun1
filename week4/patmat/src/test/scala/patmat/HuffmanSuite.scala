package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of single leaf") {
    val leaf = Leaf('a', 42)
    assert(weight(leaf) === 42)
  }

  test("weight handles null argument") {
    intercept[MatchError] {
      weight(null)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("chars of single leaf") {
    val leaf = Leaf('w', 42)
    assert(chars(leaf) === List('w'))
  }

  test("chars handles null argument") {
    intercept[MatchError] {
      chars(null)
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times for single character") {
    val result = times(List('a'))
    assert(result.size === 1)
    assert(result(0) === ('a', 1))
  }

  test("times for two characters") {
    val result = times(List('a', 'b', 'a'))
    assert(result.size === 2)
    assert(result(0) === ('a', 2))
    assert(result(1) === ('b', 1))
  }

  test("times for many characters") {
    val result = times(List('h', 'e', 'l', 'l', 'o', 'w', 'o', 'r', 'l', 'd'))
    assert(result.size === 7)
    assert(result(0) === ('d', 1))
    assert(result(1) === ('e', 1))
    assert(result(2) === ('h', 1))
    assert(result(3) === ('l', 3))
    assert(result(4) === ('o', 2))
    assert(result(5) === ('r', 1))
    assert(result(6) === ('w', 1))
  }

  test("times for no characters") {
    assert(times(List()).size === 0)
  }

  test("times handles null argument") {
    intercept[NullPointerException] {
      times(null)
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("makeOrderedLeafList for single character") {
    assert(makeOrderedLeafList(List(('t', 2))) === List(Leaf('t', 2)))
  }

  test("makeOrderedLeafList for no characters") {
    assert(makeOrderedLeafList(List()) === List())
  }

  //  test("combine of some leaf list") {
  //    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
  //    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  //  }
  //
  //  test("decode and encode a very short text should be identity") {
  //    new TestTrees {
  //      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
  //    }
  //  }
}
