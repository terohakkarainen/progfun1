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

  test("is singleton") {
    new TestTrees {
      assert(singleton(List(t1)) === true)
    }
  }

  test("is not singleton") {
    new TestTrees {
      assert(singleton(List(t1, t2)) === false)
    }
  }

  test("empty list is not singleton") {
    new TestTrees {
      assert(singleton(List()) === false)
    }
  }

  test("combine of some leaf list") {
    val leafList = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leafList) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("combine for one element list") {
    new TestTrees {
      val list = List(t1)
      assert(combine(list) === list)
    }
  }

  test("combine for two element list") {
    new TestTrees {
      val leaf1 = Leaf('e', 1)
      val leaf2 = Leaf('t', 2)
      assert(combine(List(leaf1, leaf2)) === List(Fork(leaf1, leaf2, List('e', 't'), 3)))
    }
  }

  test("combine of empty list") {
    assert(combine(List()) === List())
  }

  test("until reduces three leaves") {
    val threeLeaves = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val result = until(singleton, combine)(threeLeaves)
    assert(singleton(result))
    assert(chars(result(0)) === List('e', 't', 'x'))
    assert(weight(result(0)) === 7)
  }

  test("until reduces two leaves") {
    val twoLeaves = List(Leaf('e', 1), Leaf('t', 2))
    val result = until(singleton, combine)(twoLeaves)
    assert(singleton(result))
    assert(chars(result(0)) === List('e', 't'))
    assert(weight(result(0)) === 3)
  }

  test("until does not affect a single leaf") {
    val singleLeaf = List(Leaf('e', 1))
    val result = until(singleton, combine)(singleLeaf)
    assert(result === singleLeaf)
  }

  test("until fails for an empty list") {
    intercept[NoSuchElementException] {
      until(singleton, combine)(List())
    }
  }

  test("createCodeTree produces expected results") {
    val codeTree = createCodeTree(List('h', 'e', 'l', 'l', 'o', 'w', 'o', 'r', 'l', 'd'))
    assert(chars(codeTree) === List('e', 'h', 'r', 'w', 'd', 'o', 'l'))
    assert(weight(codeTree) === 10)
  }

  test("decoded secret") {
    assert(decodedSecret === List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a longer short text should be identity") {
    new TestTrees {
      val txt = "abddbaabddbaabddbaabddbaabddbaabddbaabddbaabddbaabddbaabddbaabddba"
      assert(decode(t2, encode(t2)(txt.toList)) === txt.toList)
    }
  }

  test("codeBits returns correct bits") {
    val codeTable = List(('a', List(0, 1, 0)), ('b', List(1, 1)))
    assert(codeBits(codeTable)('a') === List(0, 1, 0))
    assert(codeBits(codeTable)('b') === List(1, 1))
  }

  test("codeBits throws exception for non-existing char") {
    val codeTable = List(('a', List(0, 1, 0)))
    assert(codeBits(codeTable)('b') === Nil)
  }

  test("convert - t1") {
    new TestTrees {
      val codeTree = convert(t1)
      assert(codeBits(codeTree)('a') == List(0))
      assert(codeBits(codeTree)('b') == List(1))
    }
  }

  test("convert - t2") {
    new TestTrees {
      val codeTree = convert(t2)
      assert(codeBits(codeTree)('a') == List(0, 0))
      assert(codeBits(codeTree)('b') == List(0, 1))
      assert(codeBits(codeTree)('d') == List(1))
    }
  }

  test("quick encode") {
    new TestTrees {
      val txt = "abddbaabddbaabddbaabddbaabddbaabddbaabddbaabddbaabddbaabddbaabddba"
      assert(decode(t2, quickEncode(t2)(txt.toList)) === txt.toList)
    }
  }
}
