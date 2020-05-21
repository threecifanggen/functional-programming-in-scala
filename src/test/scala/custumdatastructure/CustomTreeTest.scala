package customlisttest

import org.scalatest.FunSuite

import customtree._
import customtree.CtTree._

class CtTreeTest extends FunSuite {
    val t1 = CtBranch(CtBranch(CtLeaf(1), CtBranch(CtLeaf(2), CtLeaf(2))), CtLeaf(3))
    val t2 = CtBranch(CtBranch(CtLeaf(2), CtBranch(CtLeaf(3), CtLeaf(3))), CtLeaf(4))

    test("size of t1 is 4") {
        assert(size(t1) == 4)
    }

    test("maximum of t1 is 3") {
        assert(maximum(t1) == 3)
    }

    test("depth of t1 is 4") {
        assert(depth(t1) == 4)
    }

    test("t1's element add 1 is t2") {
        assert(map(t1)(_+1) == t2)
    }
}