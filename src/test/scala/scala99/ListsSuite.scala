package scala99

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

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
    intercept[NoSuchElementException] { penultimate(List('x')) }
  }
}