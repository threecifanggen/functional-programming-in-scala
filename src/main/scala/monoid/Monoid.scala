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
        def op(a1: A => A, a2: A => a) = a1 compose a2
        def zero = (a: A) => a
    }

    //练习10.5
    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
         
    }

}