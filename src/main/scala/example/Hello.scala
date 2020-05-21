package example

import scala.annotation.tailrec

object Hello extends Greeting with App {
  println(greeting)
}

trait Greeting {
  lazy val greeting: String = "hello"
}

object easy {
  //习题2.1
  def fib(n: Int): Int = {
    @tailrec
    def fib_helper(n: Int, acc0: Int, acc1: Int): Int =
      if (n == 0) acc0
      else fib_helper(n - 1, acc1, acc1 + acc0)
    
    fib_helper(n, 0, 1)
  }

  //习题2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def isSortedHelper(as: Array[A]): Boolean = 
      if (as.size <= 1) true
      else if (!ordered(as(0), as(1))) false
      else isSortedHelper(as.tail)
    
    isSortedHelper(as)
  }

  //习题2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => ((b: B) => f(a, b))
  }

  //习题2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  //习题2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}