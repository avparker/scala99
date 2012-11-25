package scala99

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

// Unit tests liberally copied from https://github.com/danluu/ninety-nine-scala-problems
// Added some tests cases for handling exceptional cases
@RunWith(classOf[JUnitRunner])
class ListsSuite extends FunSuite {
  import scala99.Lists._

  test("P01 -- last element of list") {
    assert(last(List('x')) === 'x')
    assert(last(List(1, 2, 3)) === 3)
    assert(last(List(1, 1, 2, 3, 5, 8)) === 8)
    intercept[NoSuchElementException] { last(List()) }
  }

  test("P02 -- penultimate element of list") {
    assert(penultimate(List(1, 2)) === 1)
    assert(penultimate(List(1, 2, 3)) === 2)
    assert(penultimate(List(1, 1, 2, 3, 5, 8)) === 5)
    intercept[NoSuchElementException] { penultimate(List()) }
    intercept[NoSuchElementException] { penultimate(List(1)) }
  }

  test("P03 -- nth element of list") {
    assert(nth(0, List(1, 1, 2, 3, 5, 8)) === 1)
    assert(nth(1, List(1, 1, 2, 3, 5, 8)) === 1)
    assert(nth(2, List(1, 1, 2, 3, 5, 8)) === 2)
    assert(nth(5, List(1, 1, 2, 3, 5, 8)) === 8)
    intercept[NoSuchElementException] { nth(42, List(1, 1, 2, 3, 5, 8)) }
    intercept[NoSuchElementException] { nth(0, List()) }
  }

  test("P04 -- length of a list") {
    assert(length(List()) === 0)
    assert(length(List(1)) === 1)
    assert(length(List(1, 1, 2, 3, 5, 8)) === 6)
  }

  test("P05 -- reverse a list") {
    assert(reverse(List()) === List())
    assert(reverse(List(1)) === List(1))
    assert(reverse(List(1, 1, 2, 3, 5, 8)) === List(8, 5, 3, 2, 1, 1))
  }

  test("P06 -- isPalindrome ") {
    assert(isPalindrome(List()) === true)
    assert(isPalindrome(List(1)) === true)
    assert(isPalindrome(List(1, 2, 3, 2, 1)) === true)
    assert(isPalindrome(List(1, 2, 3, 2, 2)) === false)
    assert(isPalindrome(List(1, 2, 3, 1, 1)) === false)
    assert(isPalindrome(List(1, 2, 2, 1)) === true)
    assert(isPalindrome(List(1, 2, 1, 1)) === false)
  }

}