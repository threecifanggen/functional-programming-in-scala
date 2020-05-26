package custommonoidtest

import org.scalatest.FunSuite
import custommonoid._
import custommonoid.CustomMonoid._

class CustomMonoidTest extends FunSuite {
    test("init Addition a1 + a2 = 1 and int multiple is 0") {
        val a1 = 0
        val a2 = 1
        assert(intAddition.op(a1, a2) == 1)
        assert(intMultiplication.op(a1, a2) == 0)
    }

    test("bool op test") {
        val a1 = true
        val a2 = false
        assert(boolOr.op(a1, a2) == true)
        assert(boolAnd.op(a1, a2) == false)
    }

    test("concatenate(1, 2, 3, 4, 5) is 12345") {
        val a = List("1", "2", "3", "4", "5")
        val b = List(1, 2, 3, 4, 5)
        val c = IndexedSeq(1, 2, 3, 4, 5)
        assert(concatenate(a, stringMonoid) == "12345")
        assert(foldMap(b, stringMonoid)(_.toString) == "12345")
        assert(foldMapV(c, stringMonoid)(_.toString) == "12345")
    }
}