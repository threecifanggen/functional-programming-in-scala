package customstreamtest

import org.scalatest.FunSuite

import customstream._
import customstream.CtStream._

class CtStreamTest extends FunSuite {
    var a = CtStream(1, 2, 3, 4, 5)
    val a_list = List(1, 2, 3, 4, 5)

    test("a toList is a_list") {
        assert(a.toList == a_list)
    }

    test("take 3 of a is Stream(1, 2, 3)") {
        assert(a.take(3).toList == List(1, 2, 3))
    }
    
    test("drop 3 of a is Stream(4, 5)") {
        assert(a.drop(3).toList == List(4, 5))
    }

    test("a take while i < 3 is Stream(1, 2)") {
        assert(a.takeWhile(_ < 3).toList == List(1, 2))
    }

    test("a is all > 0") {
        assert(a.forAll(_ > 0))
    }

    test("a is not all < 3") {
        assert(!a.forAll(_ < 3))
    }

    test("contant(3) first 4 element is List(3,3,3, 3)") {
        assert(constant(3).take(4).toList == List(3, 3, 3, 3))
    }

    test("from(2) first 4 element is List(2,3,4,5)") {
        assert(from(2).take(4).toList == List(2, 3, 4, 5))
    }

    test("first 6 element of fibs is List(0, 1, 1, 2, 3, 5)") {
        assert(fibs(0, 1).take(6).toList == List(0, 1, 1, 2, 3, 5))
    }

    test("unfold(1)((x) => Some((1, 1))) is ones") {
        assert(unfold(1)((x) => Some((1, 1))).take(3).toList == ones.take(3).toList)
    }

    test("unfold(1)((x) => Some(x, x+1)) is from(1)") {
        assert(unfold(1)((x) => Some(x, x+1)).take(3).toList == from(1).take(3).toList)
    }
}