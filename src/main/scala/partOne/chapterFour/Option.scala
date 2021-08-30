package partOne.chapterFour

object Option extends App {
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case Some(a) => Some(f(a))
      case None => None
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(a) => a
      case None => default
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case Some(a) => f(a)
      case None => None
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case Some(a) => Some(a)
      case None => ob
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(a) if f(a) => Some(a)
      case _ => None
    }
  }

  case object None extends Option[Nothing]

  case class Some[+A](get: A) extends Option[A]

  object Option {
    def mean(xs: Seq[Double]): Option[Double] = {
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)
    }

    def variance(xs: Seq[Double]): Option[Double] = {
      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m, 2))))
    }

    def lift[A, B](f: A => B): Option[A] => Option[B] = a => a.map(f)

    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      (a, b) match {
        case (Some(a), Some(b)) => Some(f(a, b))
        case _ => None
      }
    }

    def _map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      a.flatMap(aa => b.map(bb => f(aa, bb)))
    }

    def correctMap2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      a.flatMap(aa => b.map(bb => f(aa, bb)))

    def forComprehensionMap2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      for {
        aa <- a
        bb <- b
      } yield f(aa, bb)
    }

    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
      a match {
        case Nil => None
        case h :: t => h.flatMap(hh => sequence(t).map(hh :: _))
      }
    }

    def simpleTraverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
      sequence(a map f)
    }

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
      a match {
        case Nil => Some(Nil)
        case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
      }
    }

    def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = {
      traverse(a)(identity)
    }
  }
}
