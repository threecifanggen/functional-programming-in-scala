package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.FunSuite

import example.easy

class HelloSpec extends AnyFlatSpec with Matchers {
  "The Hello object" should "say hello" in {
    Hello.greeting shouldEqual "hello"
  }
}

class EasyTest extends FunSuite {
  val r = 0 to 10
  val l = r.toList

  test("fib list is [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]") {
    assert(l.map(easy.fib) == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55))
    }

  test("[1, 2, 3, 4, 5] < sort is true") {
    assert(easy.isSorted(Array(1, 2, 3, 4, 5), (x: Int, y: Int) => (x < y)))
  }

  test("[] < sort is true") {
    assert(easy.isSorted(Array(), (x: Int, y: Int) => (x < y)))
  }

  test("[1] < sort is true") {
    assert(easy.isSorted(Array(1), (x: Int, y: Int) => (x < y)))
  }

  test("[1, 3, 2, 1, 4] < sort is false") {
    assert(easy.isSorted(Array(1, 3, 2, 1, 4), (x: Int, y: Int) => (x < y)) == false)
  }

  test("['asaa', 'bsas', 'cas', 'css'] size sort is false") {
    assert(easy.isSorted(Array("asaa", "bsas", "cas", "cs"), (x: String, y: String) => (x.size < y.size)) == false)
  }

  test("['asaa', 'bsas', 'cas', 'css'] size desc sort is true") {
    assert(easy.isSorted(Array("asaa", "bsas", "cas", "cs"), (x: String, y: String) => (x.size >= y.size)) == true)
  }

  val minus = (x: Int, y: Int) => (x - y)
  def curry_minus(x: Int)(y: Int): Int = x - y
  val minus_1 = (x: Int) => minus(x, 1)
  val add_3 = (x: Int) => (x + 3)

  test("minus_curry(1)(2) is -1") {
    assert(easy.curry(minus)(1)(2) == -1)
  }

  test("uncurry(curry_minus)(1, 2) == -1") {
    assert(easy.uncurry(curry_minus)(1, 2) == -1)
  }

  test("compose 1 minus 1 add 3 == 3") {
    assert(easy.compose(add_3, minus_1)(1) == 3)
  }

}