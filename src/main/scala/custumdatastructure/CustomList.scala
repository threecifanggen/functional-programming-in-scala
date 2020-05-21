package customlist

sealed trait CtList[+A]

case object Nil extends CtList[Nothing]
case class CtCons[+A](head: A, tail: CtList[A]) extends CtList[A]

object CtList {
    def sum(ints: CtList[Int]): Int = ints match {
        case Nil => 0
        case CtCons(x, xs) => x + sum(xs)
    }

    def product(ds: CtList[Double]): Double = ds match {
        case Nil => 1
        case CtCons(0.0, _) => 0.0
        case CtCons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): CtList[A] = 
        if (as.isEmpty) Nil
        else CtCons(as.head, apply(as.tail:_*))

    //练习3.2
    def tail[A](as: CtList[A]): CtList[A] = as match {
        case Nil => Nil
        case CtCons(x, xs) => xs
    }

    //练习3.3
    def setHead[A](as: CtList[A], resetHead: A): CtList[A] = as match {
        case Nil => Nil
        case CtCons(x, xs) => CtCons(resetHead, xs)
    }

    //练习3.4
    def drop[A](l: CtList[A], n: Int): CtList[A] =
        if (n == 0) l
        else drop(tail(l), n - 1)

    //练习3.5
    def dropWhile[A](l: CtList[A], f: A => Boolean): CtList[A] = l match {
        case Nil => Nil
        case CtCons(x, xs) => if (f(x)) dropWhile(xs, f) else CtCons(x, xs)
    }

    
    def append[A](a1: CtList[A], a2: CtList[A]): CtList[A] = a1 match {
        case Nil => a2
        case CtCons(x, xs) => CtCons(x, append(xs, a2))
    }

    //练习3.6
    def init[A](l: CtList[A]): CtList[A] = l match {
        case Nil => Nil
        case CtCons(x, Nil) => Nil
        case CtCons(x, xs) => CtCons(x, init(xs))
    }

    def foldRight[A, B](as: CtList[A], z: B)(f: (A, B) => B): B = as match {
        case Nil => z
        case CtCons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def sum2(ns: CtList[Int]) = foldRight(ns, 0)((x, y) => x + y)

    def product2(ns: CtList[Double]) = foldRight(ns, 1.0)(_*_)

    //练习3.9
    def length[A](as: CtList[A]): Int = foldRight(as, 0)((x, y) => y + 1)

    //练习3.10
    def foldLeft[A, B](as: CtList[A], z: B)(f: (A, B) => B): B = as match {
        case Nil => z
        case CtCons(x, Nil) => f(x, z) 
        case CtCons(x, xs) => foldLeft(xs, f(x, z))(f)
    }

    //练习3.11
    def sum3(ns: CtList[Int]) = foldLeft(ns, 0)(_+_)
    def product3(ds: CtList[Double]) = foldRight(ds, 1.0)(_*_)

    //练习3.12
    def reverse[A](l: CtList[A]): CtList[A] = foldRight(l, Nil:CtList[A])((x, y) => y match {
        case Nil => CtList(x)
        case CtCons(xx, _) => append(y, CtList(x))
    })

    //练习3.13
    //TODO
    def foldRight2[A, B](as: CtList[A], z: B)(f: (A, B) => B): B = z

    //练习3.14
    def append2[A](a1: CtList[A], a2: CtList[A]): CtList[A] = 
        foldLeft(a2, a1)((x, y) => y match {
            case Nil => a2
            case CtCons(x, xs) => CtCons(x, append2(xs, a2))
        })

    //练习3.16
    def addOne(ns: CtList[Int]): CtList[Int] = foldLeft(ns, Nil: CtList[Int])((x, ll) => ll match {
        case Nil => CtList(x+1)
        case CtCons(xx, xxs) => append(ll, CtList(x + 1))
    })

    //联系3.18
    def map[A, B](as: CtList[A])(f: A => B): CtList[B] = as match {
        case Nil => Nil
        case CtCons(xx, xxs) => CtCons(f(xx), map(xxs)(f))
    }
}

