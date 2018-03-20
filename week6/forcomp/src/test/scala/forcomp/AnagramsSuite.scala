package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  test("loadDictionary: produces something") {
    val words = loadDictionary
    assert(words.length > 1000)
  }

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }

  test("wordOccurrences: FOOBAR") {
    assert(wordOccurrences("FOOBAR") === List(('a', 1), ('b', 1), ('f', 1), ('o', 2), ('r', 1)))
  }

  test("wordOccurrences: empty string") {
    assert(wordOccurrences("") === List())
  }

  test("wordOccurrences: null string") {
    intercept[NullPointerException] {
      wordOccurrences(null)
    }
  }

  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("sentenceOccurrences: empty list") {
    assert(sentenceOccurrences(List()) === List())
  }

  test("sentenceOccurrences:  null list") {
    intercept[NullPointerException] {
      sentenceOccurrences(null)
    }
  }

  test("getDictionaryByOccurrences: small dictionary") {
    val dict = List("foo", "bar", "ofo")
    val dictByOcc = getDictionaryByOccurrences(dict)
    assert(dictByOcc.size === 2)
    assert(dictByOcc(List(('f', 1), ('o', 2))) === List("foo", "ofo"))
    assert(dictByOcc(List(('a', 1), ('b', 1), ('r', 1))) === List("bar"))
  }

  test("dictionaryByOccurrences: large dictionary") {
    val dict = dictionaryByOccurrences
    assert(dict.size > 1000)
    assert(dict(List(('l', 1), ('u', 2), ('z', 1))) === List("Zulu"))
  }

  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }

  test("dictionaryByOccurrences: empty dictionary") {
    val dictByOcc = getDictionaryByOccurrences(List())
    assert(dictByOcc.size === 0)
  }

  test("dictionaryByOccurrences: null dictionary") {
    intercept[NullPointerException] {
      getDictionaryByOccurrences(null)
    }
  }

  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("word anagrams: empty string") {
    assert(wordAnagrams("").size === 0)
  }

  test("word anagrams: null string") {
    intercept[NullPointerException] {
      wordAnagrams(null)
    }
  }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract: empty list") {
    assert(subtract(List(), List()) === List())
  }

  test("subtract: subtract on list itself") {
    val list = List(('a', 1), ('g', 3), ('r', 42))
    assert(subtract(list, list) === List())
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("combinations: null ") {
    intercept[NullPointerException] {
      combinations(null)
    }
  }

  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

  test("sentence anagrams: null ") {
    intercept[NullPointerException] {
      sentenceAnagrams(null)
    }
  }
}
