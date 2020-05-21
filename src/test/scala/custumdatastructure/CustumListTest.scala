package example

import org.scalatest.FunSuite

import customlist._
import customlist.CtList._

class CtListTest extends FunSuite {
    val a = CtList(1, 2, 3, 4, 5, 6, 7)
    val b = CtList(1.0, 2.0, 3.0, 4.0, 5.0, 6.1, 7.3)
    val c = CtList(7, 6, 5, 4, 3, 2, 1)

    test("length of a is 7") {
        assert(length(a) == 7)
    }

    test("foldLeft(a, 1)(_+_) == 29") {
        assert(foldLeft(a, 1)(_ + _) == 29)
    }

    test("folsLeft(Nil, 1)(_+_) == 1") {
        assert(foldLeft(Nil: CtList[Int], 1)(_ + _) == 1)
    }

    test("sum3(a) == 28") {
        assert(sum3(a) == 28)
    }

    test("product3(b) == product(b)") {
        assert(product3(b) == product(b))
    }
    
    test("reverse(a) == c") {
        assert(reverse(a) == c)
    }

    test("a add 1 is CtList(2, 3, 4, 5, 6, 7, 8)") {
        assert(addOne(a) == CtList(2, 3, 4, 5, 6, 7, 8))
    }
}

