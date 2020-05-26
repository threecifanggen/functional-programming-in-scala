package customtree

sealed trait CtTree[+A]
case class CtLeaf[A](value: A) extends CtTree[A]
case class CtBranch[A](left: CtTree[A], right: CtTree[A]) extends CtTree[A]

object CtTree {
    //习题3.25
    def size[A](t: CtTree[A]): Int = t match {
        case CtLeaf(_) => 1
        case CtBranch(l, r) => size(l) + size(r)
    }

    //习题3.26
    def maximum(it: CtTree[Int]): Int = it match {
        case CtLeaf(i) => i
        case CtBranch(l, r) => if (maximum(l) > maximum(r)) maximum(l) else maximum(r) 
    }

    //习题3.27
    def depth[A](t: CtTree[A]): Int = t match {
        case CtLeaf(_) => 1
        case CtBranch(l, r) => if (depth(l) > depth(r)) 1 + depth(l) else 1 + depth(r) 
    }

    //习题3.28
    def map[A, B](t: CtTree[A])(f: A => B): CtTree[B] = t match {
        case CtLeaf(i) => CtLeaf(f(i))
        case CtBranch(left, right) => CtBranch(map(left)(f), map(right)(f))
    }

    //习题3.29
    //TODO
}