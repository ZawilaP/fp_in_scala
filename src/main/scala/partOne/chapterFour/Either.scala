package partOne.chapterFour

object Either extends App {
  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(value) => Right(f(value))
      case Left(value) => Left(value)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(value) => f(value)
      case Left(value) => Left(value)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(_) => b
      case Left(value) => Left(value)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
        case (Right(value1), Right(value2)) => Right(f(value1, value2))
        case (Left(value), _) => Left(value)
      }
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  object Either {
    def mean(xs: IndexedSeq[Double]): Either[String, Double] =
      if (xs.isEmpty) Left("Mean of empty list!")
      else Right(xs.sum/xs.length)

    def safeDiv(x: Int, y: Int): Either[Exception, Int] =
      try Right(x / y)
      catch { case e: Exception => Left(e)}

    def Try[A](a: => A): Either[Exception, A] =
      try Right(a)
      catch {case e: Exception => Left(e)}

    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
      es match {
        case Nil => Right(Nil)
        case h :: t => h.flatMap(hh => sequence(t) map (hh :: _))
      }
    }

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      as match {
        case Nil => Right(Nil)
        case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
      }
    }

    def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
      traverse(es)(identity)
    }
  }
}
