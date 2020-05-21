package listenhencetest

import org.scalatest.FunSuite
import listenhence.ListEnhence._

class ListEnhenceTest extends FunSuite {
    test("List(1, 2, 3, 4, 5) has List(2, 3)") {
        assert(hasSubsequence(List(1, 2, 3, 4, 5), List(2, 3)))
    }
    test("List(1, 2, 3, 4, 5) has not List(2, 3)") {
        assert(hasSubsequence(List(1, 2, 3, 4, 5), List(1, 3)) == false)
    }
}