package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelParenthesesBalancing._

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {

  test("balance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  test("balance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("balance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  test("combine works as expected") {
    def check(input: ((Int, Int), (Int, Int)), expected: (Int, Int)) =
      assert(combine(input._1, input._2) == expected)

    check(((0, 0), (0, 0)), (0, 0))
    check(((0, 1), (1, 0)), (0, 0))
    check(((0, 5), (4, 0)), (0, 1))
    check(((1, 0), (0, 0)), (1, 0))
    check(((1, 0), (0, 0)), (1, 0))
    check(((9, 1), (1, 0)), (9, 0))
    check(((0, 1), (2, 1)), (9999, 9999))
  }

  test("parBalance should work for string of length 2 and threshold 1") {
    assert(parBalance(Array('(', ')'), 1) == true)
  }
}