package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * This class is a test suite for the methods in object FunSets. To run
  * the test suite, you can either:
  *  - run the "test" command in the SBT console
  *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
  */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
    * Link to the scaladoc - very clear and detailed tutorial of FunSuite
    *
    * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
    *
    * Operators
    *  - test
    *  - ignore
    *  - pending
    */

  /**
    * Tests are written using the "test" operator and the "assert" method.
    */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
    * For ScalaTest tests, there exists a special equality operator "===" that
    * can be used inside "assert". If the assertion fails, the two values will
    * be printed in the error message. Otherwise, when using "==", the test
    * error message will only say "assertion failed", without showing the values.
    *
    * Try it out! Change the values so that the assertion fails, and look at the
    * error message.
    */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
    * When writing tests, one would often like to re-use certain values for multiple
    * tests. For instance, we would like to create an Int-set and have multiple test
    * about it.
    *
    * Instead of copy-pasting the code for creating the set into every test, we can
    * store it in the test class using a val:
    *
    * val s1 = singletonSet(1)
    *
    * However, what happens if the method "singletonSet" has a bug and crashes? Then
    * the test methods are not even executed, because creating an instance of the
    * test class fails!
    *
    * Therefore, we put the shared values into a separate trait (traits are like
    * abstract classes), and create an instance inside each test method.
    *
    */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
    * This test is currently disabled (by using "ignore") because the method
    * "singletonSet" is not yet implemented and the test would fail.
    *
    * Once you finish your implementation of "singletonSet", exchange the
    * function "ignore" by "test".
    */
  test("singletonSet(1) contains 1") {

    /**
      * We create a new instance of the "TestSets" trait, this gives us access
      * to the values "s1" to "s3".
      */
    new TestSets {
      /**
        * The string argument of "assert" is a message that is printed in case
        * the test fails. This helps identifying which assertion failed.
        */
      assert(contains(s1, 1))
      assert(!contains(s1, 2))
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val shouldContainOneAndTwo = union(s1, s2)
      assert(contains(shouldContainOneAndTwo, 1))
      assert(contains(shouldContainOneAndTwo, 2))
      assert(!contains(shouldContainOneAndTwo, 3))
    }
  }

  test("complement contains all elements not in set") {
    new TestSets {
      val shouldContainAnythingButOne = comp(s1)
      assert(!contains(shouldContainAnythingButOne, 1))
      assert(contains(shouldContainAnythingButOne, 2))
      assert(contains(shouldContainAnythingButOne, 3))
    }
  }

  test("intersection contains elements in both sets") {
    new TestSets {
      val shouldBeEmpty = intersect(s1, s2)
      assert(!contains(shouldBeEmpty, 1))
      assert(!contains(shouldBeEmpty, 2))
      assert(!contains(shouldBeEmpty, 3))

      val shouldContainOne = intersect(s1, s1)
      assert(contains(shouldContainOne, 1))
      assert(!contains(shouldContainOne, 2))
    }
  }

  test("difference contains elements in first set only") {
    new TestSets {
      val shouldContainOne = diff(s1, s2)
      assert(contains(shouldContainOne, 1))
      assert(!contains(shouldContainOne, 2))
    }
  }

  test("filter only returns elements accepted by predicate") {
    new TestSets {
      val shouldContainOne = filter(s1, (x: Int) => x == 1)
      assert(contains(shouldContainOne, 1))
      assert(!contains(shouldContainOne, 2))

      val shouldContainOneAndTwo = union(s1, s2)
      val shouldContainOneTwoThree = union(shouldContainOneAndTwo, s3)
      val shouldContainThree = filter(shouldContainOneTwoThree, (x: Int) => x > 2)
      assert(contains(shouldContainThree, 3))
      assert(!contains(shouldContainThree, 2))
    }
  }

  test("forall tests predicate against all elements in the set") {
    new TestSets {
      val containsOne = s1
      assert(forall(containsOne, (x: Int) => x > 0))
      assert(!forall(containsOne, (x: Int) => x != 1))

      val containsOneAndThree = union(s1, s3)
      assert(!forall(containsOneAndThree, (x: Int) => x == bound))
      assert(forall(containsOneAndThree, (x: Int) => x != 2))

      val containsBounds = union(singletonSet(bound), singletonSet(-bound))
      assert(forall(containsBounds, (x: Int) => Math.abs(x) == bound))

      val containsOnlyOverBound = singletonSet(bound + 1)
      assert(forall(containsOnlyOverBound, (x: Int) => x == bound + 1))

      val containsOnlyBelowBound = singletonSet(-bound - 1)
      assert(forall(containsOnlyBelowBound, (x: Int) => x == -bound - 1))
    }
  }

  test("exists tests whether there's at least one element fulfilling the predicate") {
    new TestSets {
      val containsOneAndTwo = union(s1, s2)
      assert(exists(containsOneAndTwo, (x: Int) => x == 1))
      assert(!exists(containsOneAndTwo, (x: Int) => x == 3))
    }
  }

  test("map tranforms all set elements to new value") {
    new TestSets {
      val containsOneAndTwo = union(s1, s2)
      val shouldContainTwoAndThree = map(containsOneAndTwo, (x: Int) => x + 1)
      assert(contains(shouldContainTwoAndThree, 2))
      assert(contains(shouldContainTwoAndThree, 3))
      assert(!contains(shouldContainTwoAndThree, 1))
    }
  }
}
