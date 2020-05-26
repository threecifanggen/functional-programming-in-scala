package custommonoid

trait CustomMonoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
}

object CustomMonoid {
    val stringMonoid = new CustomMonoid[String] {
        def op(a1: String, a2: String) = a1 + a2
        def zero = "" 
    }
    
    def listMonoid[A] = new CustomMonoid[List[A]] {
        def op(a1: List[A], a2: List[A]) = a1 ++ a2
        def zero = Nil
    }

    //练习10.1
    val intAddition = new CustomMonoid[Int] {
        def op(a1: Int, a2: Int) = a1 + a2
        def zero = 0
    }

    val intMultiplication = new CustomMonoid[Int] {
        def op(a1: Int, a2: Int) = a1 * a2
        def zero = 1
    }

    val boolOr = new CustomMonoid[Boolean] {
        def op(a1: Boolean, a2: Boolean) = a1 || a2
        def zero = false
    }

    val boolAnd = new CustomMonoid[Boolean] {
        def op(a1: Boolean, a2: Boolean) = a1 && a2
        def zero = true
    }

    //练习10.2
    def optionMonoid[A] = new CustomMonoid[Option[A]] {
        def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
        def zero = None
    }

    //练习10.3
    def endMonoid[A] = new CustomMonoid[A => A] {
        def op(a1: A => A, a2: A => A) = a1 compose a2
        def zero = (a: A) => a
    }

    def concatenate[A](as: List[A], m: CustomMonoid[A]): A = as.foldLeft(m.zero)(m.op)

    //练习10.5
    def foldMap[A, B](as: List[A], m: CustomMonoid[B])(f: A => B): B = {
        concatenate(as.map(f), m) 
    }

    //练习10.7
    def foldMapV[A, B](v: IndexedSeq[A], m: CustomMonoid[B])(f: A => B): B = {
        if (v.size == 1)
            f(v.head)
        else if (v.size == 0) {
            m.zero
        }
        else {
            val (l, r) = v.splitAt(v.length / 2)
            m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
        }
    }
}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

object WC {
    val wcMonoid = new CustomMonoid[WC] {
        def op(a1: WC, b1: WC) = (a1, b1) match {
            case (Stub(a), Stub(b)) => Stub(a+b)
            case (Stub(a), Part(l, w, r)) => Part(l+a, w, r)
            case (Part(l, w, r), Stub(b)) => Part(l, w, r+b)
            case (Part(l1, w1, r1), Part(l2, w2, r2)) => 
                Part(l1, w1 + (if ((l1 + l2).isEmpty) 0 else 0) + w2, r2) 
        }

        def zero: WC = Stub("")
    }

    // def countWords(s: String): Int = {
    //     def wc(c: Char): WC =
    //         if (c.isWhitespace)
    //             Part("", 0, "")
    //         else
    //             Stub(c.toString)
    //         // `unstub(s)` is 0 if `s` is empty, otherwise 1.
    //     def unstub(s: String) = s.length min 1
    //     foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
    //         case Stub(s) => unstub(s)
    //         case Part(l, w, r) => unstub(l) + w + unstub(r)
    //     }
    // }
}