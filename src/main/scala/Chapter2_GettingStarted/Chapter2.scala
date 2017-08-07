package Chapter2_GettingStarted

import scala.annotation.tailrec

object Chapter2 {

  // exercise 2.1
  def getNthFibonacciNum(n: Int): BigInt = {
    @tailrec
    def getFibNum(n: Int, last: BigInt, next: BigInt): BigInt = n match {
        case 0 => last
        case 1 => next
        case _ => getFibNum(n - 1, next, last + next)
    }
    if (n < 1) -1 else getFibNum(n, 0, 1)
  }

  // exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def areEqual(index: Int, as: Array[A], ordered: (A, A) => Boolean): Boolean = index match {
      case _ if index == as.length - 1 => true
      case _ => if (ordered(as(index), as(index + 1))) areEqual(index + 1, as, ordered) else false
    }
    areEqual(0, as, ordered)
  }

  // exercise 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => b => f(a,b)

  // exercise 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a,b) => f(a)(b)

  // exercise 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
}
