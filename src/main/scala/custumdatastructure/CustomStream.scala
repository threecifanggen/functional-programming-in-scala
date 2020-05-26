package customstream

import scala.annotation._

sealed trait CtStream[+A] {
    //练习5.1
    def toList: List[A] = {
        @tailrec
        def helper(s: CtStream[A], acc: List[A]): List[A] = s match {
            case CtCons(h, t) => helper(t(), h()::acc)
            case _ => acc
        }
        helper(this, List()).reverse
    }

    //练习5.2 - take
    def take(n: Int): CtStream[A] = this match {
        case CtCons(h, t) if (n > 1) => CtStream.cons(h() , t().take(n - 1))
        case CtCons(h, _) if (n == 1) => CtStream.cons(h(), CtStream.empty)
        case _ => CtStream.empty
    }

    //练习5.2 - drop
    def drop(n: Int): CtStream[A] = this match {
        case CtCons(h, t) if (n > 1) => t().drop(n - 1)
        case CtCons(h, t) if (n == 1) => t()
        case _ => CtStream.empty
    }

    //练习5.3
    def takeWhile(p: A => Boolean): CtStream[A] = this match {
        case CtCons(h, t) if (p(h())) => CtStream.cons(h(), t().takeWhile(p))
        case CtCons(h, t) if (!p(h())) => CtStream.empty
        case _ => CtStream.empty
    }

    //练习5.4
    def forAll(p: A => Boolean): Boolean = this match {
        case CtCons(h, t) if (!p(h())) => false
        case CtCons(h, t) => t().forAll(p)
        case _ => true
    }
}

case object CtEmpty extends CtStream[Nothing]
case class CtCons[+A](h: () => A, t: () => CtStream[A]) extends CtStream[A]

object CtStream {
    def cons[A](hd: => A, tl: => CtStream[A]): CtStream[A] = {
        lazy val head = hd
        lazy val tail = tl
        CtCons(() => head, () => tail)
    }

    def empty[A]: CtStream[A] = CtEmpty

    def apply[A](as: A*): CtStream[A] = 
        if (as.isEmpty) empty else cons(as.head, apply(as.tail:_*))

    val ones: CtStream[Int] = CtStream.cons(1, ones)

    def constant[A](a: A): CtStream[A] = CtStream.cons(a, constant(a))

    def from(n: Int): CtStream[Int] = CtStream.cons(n, from(n + 1))

    def fibs(a0: Int, a1: Int): CtStream[Int] = 
        CtStream.cons(a0, fibs(a1, a0 + a1))

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): CtStream[A] = f(z) match {
        case Some((a, s)) => CtStream.cons(a, unfold(s)(f))
        case _ => CtStream.empty
    }
}