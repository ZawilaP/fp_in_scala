package partOne.chapterThree

import scala.annotation.tailrec

object List extends App {
  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def tail[A](l: List[A]): List[A] = {
      l match {
        case Nil => Nil
        case Cons(_, xs) => xs
      }
    }

    def setHead[A](el: A, l: List[A]): List[A] = {
      Cons(el, tail(l))
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      @tailrec
      def helper(n: Int, acc: List[A]): List[A] = {
        n match {
          case 0 => acc
          case _ => helper(n - 1, tail(acc))
        }
      }

      helper(n, l)
    }

    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
      @tailrec
      def helper(acc: List[A]): List[A] = {
        acc match {
          case Cons(x, xs) if f(x) => helper(xs)
          case _ => acc
        }
      }

      helper(l)
    }

    def init[A](l: List[A]): List[A] = {
      l match {
        case Nil => Nil
        case Cons(_, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
      }
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }
    }

    def sum2(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)
    def product2(ns: List[Int]): Int = foldRight(ns, 1)(_ * _)
    def length[T](ns: List[T]): Int = foldRight(ns, 0)((_, b) => b + 1)

    @tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
      as match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      }
    }

    def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)
    def product3(ns: List[Int]): Int = foldLeft(ns, 0)(_ * _)
    def length2[T](ns: List[T]): Int = foldLeft(ns, 0)((b, _) => b + 1)
    def reverse[T](ns: List[T]): List[T] = foldLeft(ns, Nil: List[T])((a, b) => Cons(b, a))

    def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      foldLeft(reverse(as), z)((b, a) => f(a, b))
    }

    def append[A](a1: List[A], a2: List[A]): List[A] = foldRightViaFoldLeft(a1, a2)(Cons(_, _))

    def append2[A](a1: List[A], a2: List[A]): List[A] = reverse(foldLeft(a1, a2)((a, b) => Cons(b, a)))

    def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])((a, b) => append(a, b))

    def add1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

    def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

    def map[A, B](as: List[A])(f: A => B): List[B] = {
      foldRightViaFoldLeft(as, Nil: List[B])((h, t) => Cons(f(h), t))
    }

    def filter[A](as: List[A])(f: A => Boolean): List[A] = {
      as match {
        case Nil => Nil
        case Cons(h, t) if f(h) => filter(t)(f)
        case Cons(h, t) => Cons(h, filter(t)(f))
      }
    }

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
      concat(map(as)(f))
    }

    def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
      flatMap[A, A](as)(a => if (f(a)) Cons(a, Nil) else Nil)
    }

    def addTwoLists(l1: List[Int], l2: List[Int]): List[Int] = {
      require(length(l1) == length(l2))

      (l1, l2) match {
        case (Nil, Nil) => Nil
        case (Cons(h, t), Cons(x, xs)) => Cons(h + x, addTwoLists(t, xs))
      }
    }

    def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = {
      require(length(a) == length(b))
      (a, b) match {
        case (Nil, Nil) => Nil
        case (Cons(h, t), Cons(x, xs)) => Cons(f(h, x), zipWith(t, xs)(f))
      }
    }

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      @tailrec
      def helper(initialSup: List[A], leftSub: List[A], acc: List[A]): List[A] = {
        (initialSup, leftSub) match {
          case (Nil, _) => acc
          case (_, Nil) => acc
          case (Cons(h, t), Cons(x, xs)) if (h == x) => helper(t, xs, Cons(x, acc))
          case (Cons(h, t), Cons(x, _)) if (h != x) => helper(t, leftSub, Nil)
        }
      }

      val helperResult = reverse(helper(sup, sub, Nil))

      if (helperResult == sub) true else false
    }
  }
}
