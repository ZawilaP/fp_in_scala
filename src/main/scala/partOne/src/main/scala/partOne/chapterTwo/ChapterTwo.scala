package partOne.chapterTwo

import scala.annotation.tailrec

object ChapterTwo extends App {
  def fib(n: Int): Int = {
    require(n > 0)

    @tailrec
    def go(n: Int, currentNum: Int, nextNum: Int): Int = {
      if (n == 0) currentNum
      else go(n - 1, nextNum, currentNum + nextNum)
    }

    if (n == 1) 0
    else if (n == 2) 1
    else go(n, 0, 1)
  }

  println(s"Showing first Fibonacci number: ${fib(1)}")
  println(s"Showing second Fibonacci number: ${fib(2)}")
  println(s"Showing 5th Fibonacci number: ${fib(5)}")
  println(s"Showing 10th Fibonacci number: ${fib(10)}")

  /**
   * **********************************************************************************
   */

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length) true
      else if (ordered(as(n - 1), as(n))) loop(n + 1)
      else false
    }

    loop(1)
  }

  def alphabeticalOrder(a: String, b: String): Boolean = a < b

  def intOrder(a: Int, b: Int): Boolean = a < b

  val correctStringArray: Array[String] = Array("a", "b", "g", "k")
  val incorrectStringArray: Array[String] = Array("z", "b", "a", "k")
  val correctIntArray: Array[Int] = Array(1, 2, 5, 11)
  val incorrectIntArray: Array[Int] = Array(11, 4, 2, 8)

  assert(isSorted(correctStringArray, alphabeticalOrder) && isSorted(correctIntArray, intOrder)
    && !isSorted(incorrectStringArray, alphabeticalOrder) && !isSorted(incorrectIntArray, intOrder))

  /**
   * **********************************************************************************
   */

  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a => b => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  /**
   * **********************************************************************************
   */

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
