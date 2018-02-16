package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {

  import Main.countChange

  test("countChange: example given in instructions") {
    assert(countChange(4, List(1, 2)) === 3)
  }

  test("countChange: sorted CHF") {
    assert(countChange(300, List(5, 10, 20, 50, 100, 200, 500)) === 1022)
  }

  test("countChange: no pennies") {
    assert(countChange(301, List(5, 10, 20, 50, 100, 200, 500)) === 0)
  }

  test("countChange: unsorted CHF") {
    assert(countChange(300, List(500, 5, 50, 100, 20, 200, 10)) === 1022)
  }

  test("no money, nothing to change") {
    assert(countChange(0, List(1, 2)) === 0)
  }

  test("no coins, cannot change") {
    assert(countChange(3, List()) === 0)
  }

  test("negative money is not accepted") {
    intercept[AssertionError] {
      countChange(-1, List(1, 2))
    }
  }

  test("zero or negative coins not accepted") {
    intercept[AssertionError] {
      countChange(1, List(0))
    }
    intercept[AssertionError] {
      countChange(1, List(-1))
    }
  }

  test("additional tests") {
    assert(countChange(1, List(1)) === 1)
    assert(countChange(1, List(2)) === 0)
  }
}
