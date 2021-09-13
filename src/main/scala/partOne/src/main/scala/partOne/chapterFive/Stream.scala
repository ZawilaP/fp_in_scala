package partOne.chapterFive

import partOne.chapterFive.Stream.Stream.{cons, empty, unfold}

import scala.annotation.tailrec

object Stream extends App {
  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }

    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

    def toListTailRecursive: List[A] = {
      @annotation.tailrec
      def helper(s: Stream[A], acc: List[A]): List[A] = s match {
        case Cons(h, t) => helper(t(), h() :: acc)
        case _ => acc
      }

      helper(this, List()).reverse
    }

    def take(n: Int): Stream[A] = this match {
      case Empty => empty
      case Cons(h: A, t) => cons(h(), t().take(n - 1))
    }

    def drop(n: Int): Stream[A] = {
      @tailrec
      def helper(n: Int, acc: Stream[A]): Stream[A] = {
        (n, acc) match {
          case (0, _) => acc
          case (_, Cons(_, t)) => helper(n - 1, t())
        }
      }

      helper(n, this)
    }

    def dropBetter(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().dropBetter(n - 1)
      case _ => this
    }

    def takeWhile(p: A => Boolean): Stream[A] = {
      def helper(acc: Stream[A]): Stream[A] = {
        acc match {
          case Empty => empty
          case Cons(h: A, t) if p(h()) => cons(h, helper(t()))
        }
      }

      helper(this)
    }

    def takeWhileBetter(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => cons(h(), t() takeWhileBetter p)
      case _ => empty
    }

    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def existsViaFoldRight(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean = this match {
      case Empty => true
      case Cons(h, _) if !p(h()) => false
      case Cons(_, t) => t().forAll(p)
    }

    def forAllViaFoldRight(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) && b)

    def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

    def headOptionViaFoldRight: Option[A] =
      foldRight(None: Option[A])((h, _) => Some(h))

    def map[B](f: A => B): Stream[B] =
      foldRight(Empty: Stream[B])((a,b) => cons(f(a), b))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else b)

    def append[B >: A](s1: Stream[B]): Stream[B] =
      foldRight(s1)(cons(_, _))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(Empty: Stream[B])((a, b) => f(a) append b)

    def find(p: A => Boolean): Option[A] =
      filter(p).headOption

    def mapViaUnfold[B](f: A => B): Stream[B] =
      unfold[B, Stream[A]](this) {
        case Cons(a, b) => Some((f(a()), b()))
        case _ => None}

    def takeViaUnfold(n: Int): Stream[A] =
      unfold((this, n)) {
        case (Cons(h, _), 1) => Some((h(), (empty, 0)))
        case (Cons(h,t), n) if n > 1 => Some((h(), (t(), n-1)))
        case (Empty, _) => None
      }

    def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
      unfold(this) {
        case Cons(h, t) if p(h()) => Some((h(), t()))
        case _ => None
      }

    def zipWithViaUnfold[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] =
      unfold((this, b)) {
        case (Cons(h, t), Cons(x, xs)) => Some((f(h(), x()), (t(), xs())))
        case _ => None
      }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
      zipWithAll(s2)((_, _))

    def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
      unfold((this, s2)) {
        case (Empty, Empty) => None
        case (Cons(h, t), Empty) => Some((f(Some(h()), None), (t(), empty)))
        case (Empty, Cons(h, t)) => Some((f(None, Some(h())), (empty, t())))
        case (Cons(h, t), Cons(x, xs)) => Some((f(Some(h()), Some(x())), (t(), xs())))
      }

    def startsWith[B >: A](s: Stream[B]): Boolean =
      zipAll(s).takeWhile(_._2.isDefined) forAll {
        case (a,b) => a == b
      }

    def tails: Stream[Stream[A]] =
      unfold(this) {
        case Cons(h, t) => Some((Cons(h, t), t()))
        case Empty => None
      } append Stream(empty)

    def hasSubsequence[B >: A](s: Stream[B]): Boolean =
      tails exists (_ startsWith s)

    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
      foldRight((z, Stream(z)))((a, s) => { // in case of Empty it will return Stream of given neutral value
        lazy val b = s // assignment to ensure only evaluating this once and not during every call, previously I didn't do it...
        val b2 = f(a, b._1)
        (b2, cons(b2, b._2)) // accumulate sums of next values
      })._2
  }
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    def from(n: Int): Stream[Int] = cons(n, from(n+1))

    def fibs: Stream[Int] = {
      def helper(f1: Int, f2: Int): Stream[Int] = cons(f1, helper(f2, f1 + f2))

      helper(0, 1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A , S)]): Stream[A] =
      f(z) match {
        case Some((a, b)) => cons(a, unfold(b)(f))
        case None => empty
      }

    def fibsViaUnfold: Stream[Int] =
      unfold[Int, (Int, Int)]((0, 1)){ case (a, b) => Some((a, (b, a+b)))}

    def fromViaUnfold(n: Int): Stream[Int] =
      unfold(n)(n => Some((n, n+1)))

    def constantViaUnfold[A](a: A): Stream[A] =
      unfold(a)(a => Some((a, a)))

    def onesViaUnfold: Stream[Int] =
      unfold(1)(_ => Some((1, 1)))
  }

  println(Stream(1, 2, 3).take(2).toListTailRecursive)
}
