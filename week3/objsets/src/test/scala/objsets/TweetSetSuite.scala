package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {

  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: user 'a' once in set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: two times 20 retweets in set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("filter: predicate not holding for any component") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets > 10000)) === 0)
    }
  }

  test("union: set4c with set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: set5 with empty set") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: empty set with s5") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("union: empty set with itself") {
    new TestSets {
      assert(size(set1.union(set1)) === 0)
    }
  }

  test("union: non-empty set with itself") {
    new TestSets {
      assert(size(set5.union(set5)) === size(set5))
    }
  }

  test("most retweeted can be found in one element set") {
    new TestSets {
      assert(set2.mostRetweeted.text === "a body")
    }
  }

  test("most retweeted can be found in two element set") {
    new TestSets {
      val most = new Tweet("foo", "bar", 100)
      val mostSet = set2.incl(most)
      assert(mostSet.mostRetweeted.text === "bar")
    }
  }

  test("most retweeted can be found in larger set") {
    new TestSets {
      assert(set5.mostRetweeted.retweets === 20)
    }
  }

  test("most retweeted not available in empty set") {
    new TestSets {
      intercept[NoSuchElementException] {
        set1.mostRetweeted
      }
    }
  }

  test("descending: empty set") {
    new TestSets {
      assert(set1.descendingByRetweet.isEmpty)
    }
  }

  test("descending: one element set") {
    new TestSets {
      val list = set2.descendingByRetweet
      assert(list.head.retweets === 20)
      assert(list.tail.isEmpty)
    }
  }

  test("descending: two element set") {
    new TestSets {
      val list = set3.descendingByRetweet
      assert(list.head.retweets === 20)
      assert(list.tail.head.retweets === 20)
      assert(list.tail.tail.isEmpty)
    }
  }

  test("descending: larger set") {
    new TestSets {
      val most = new Tweet("foo", "bar", 100)
      val largerSet = set5.incl(most)
      val list = largerSet.descendingByRetweet
      assert(list.head.retweets === 100)
      assert(list.tail.head.retweets === 20)
      assert(list.tail.tail.head.retweets === 20)
      assert(list.tail.tail.tail.head.retweets === 9)
      assert(list.tail.tail.tail.tail.head.retweets === 7)
      assert(list.tail.tail.tail.tail.tail.isEmpty)
    }
  }

  test("descending: set5") {
    new TestSets {
      val list = set5.descendingByRetweet
      assert(!list.isEmpty)
      assert(list.head.user == "a" || list.head.user == "b")
    }
  }

  test("googleTweets & appleTweets: something got") {
    new TestSets {
      assert(size(GoogleVsApple.googleTweets) > 0)
      assert(size(GoogleVsApple.appleTweets) > 0)
    }
  }
}
