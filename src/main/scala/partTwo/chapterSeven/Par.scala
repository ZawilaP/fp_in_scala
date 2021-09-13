package partTwo.chapterSeven

import partTwo.chapterSeven.Par.Par.{countWordsByTraverse, maxByTraverse, sumByTraverse}

import java.util.concurrent.{Callable, ExecutorService, Executors, Future, TimeUnit}

object Par extends App {
  type Par[A] = ExecutorService => Future[A]

  sealed trait Monoid[A] {
    val neutral: A
    def compose(a: A,b: A): A
  }

  implicit class MonoidOps[A](value:A) {
    def compose(that: A)(implicit monoid: Monoid[A]): A = {
      monoid.compose(value, that)
    }
  }

  implicit val intSumMonoid: Monoid[Int] = new Monoid[Int] {
    val neutral = 0
    override def compose(a: Int, b: Int): Int = a + b
  }

  implicit val intMaxMonoid: Monoid[Int] = new Monoid[Int] {
    val neutral: Int = Int.MinValue
    override def compose(a: Int, b: Int): Int = a max b
  }

  object Par {
    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true

      def get(timeout: Long, units: TimeUnit): A = get

      def isCancelled = false

      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
      (es: ExecutorService) => {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
      }
    }

    //    def map2Timeout[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???

    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        def call: A = a(es).get
      })

    def asyncF[A, B](f: A => B): A => Par[B] = a => unit(f(a))

    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a, _) => f(a))

    def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

    def sequence[A](fs: List[Par[A]]): Par[List[A]] =
      fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
      sequence(ps.map(asyncF(f)))
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
      map(sequence(as.map(asyncF(a => if (f(a)) List(a) else List()))))(_.flatten)

    def sum(ints: IndexedSeq[Int]): Par[Int] =
      if (ints.length <= 1) unit(ints.headOption getOrElse 0)
      else {
        val (l, r) = ints.splitAt(ints.length / 2)
        map2(fork(sum(l)), fork(sum(r)))(_ + _)
      }

    def traverse[A, B](as: Traversable[A])(f: Option[A] => Option[B])(ev: Monoid[B]): Par[B] =
      if (as.size <= 1) unit(f(as.headOption) getOrElse ev.neutral)
      else {
        val (l, r) = as.splitAt(as.size / 2)
        map2(fork(traverse(l)(f)(ev)), fork(traverse(r)(f)(ev)))(ev.compose)
      }

    def sumByTraverse(ints: IndexedSeq[Int]): Par[Int] =
      traverse(ints)(identity)(intSumMonoid)

    def maxByTraverse(ints: IndexedSeq[Int]): Par[Int] =
      traverse(ints)(identity)(intMaxMonoid)

    def countWords(as: List[String]): Par[Int] =
      if (as.length <= 1) {
        val res = as.headOption match {
          case None => Some(0)
          case Some(h) => Some(h.split(" ").length)
        }
        unit(res.get)
      } else {
        val (l, r) = as.splitAt(as.length / 2)
        map2(fork(countWords(l)), fork(countWords(r)))(_ + _)
      }

    def countWordsByTraverse(as: List[String]): Par[Int] =
      traverse(as){ case None => None: Option[Int]
      case Some(h) => Some(h.split(" ").length)}(intSumMonoid)

    def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
      val f1: (A, B) => C => D = (a: A, b: B) => (c: C) => f(a, b, c)
      val f2: Par[C => D] = map2(a, b)(f1)
      map2(f2, c)((g: C => D, c: C) => g(c))
    }

    def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] = {
      val f1: (A, B) => C => D => E = (a: A, b: B) => (c: C) => (d: D) => f(a,b,c,d)
      val f2: Par[C => D => E] = map2(a, b)(f1)
      val f3: Par[D => E] = map2(f2, c)((g: C => D => E, c: C) => g(c))
      map2(f3, d)((g: D => E, d: D) => g(d))
    }

    def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] = {
      val f1: (A, B) => C => D => E => F = (a1: A, b1:B) => (c1: C) => (d1: D) => (e1: E) => f(a1, b1, c1, d1, e1)
      val f2: Par[C => D => E => F] = map2(a, b)(f1)
      val f3: Par[D => E => F] = map2(f2, c)((g: C => D => E => F, c1: C) => g(c1))
      val f4: Par[E => F] = map2(f3, d)((g: D => E => F, d1: D) => g(d1))
      map2(f4, e)((g: E => F, e1: E) => g(e1))
    }

    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get

    /**
     * Exercise 7.7
     *
     * Given: map(y)(id) == y
     * Prove: map(map(y)(g))(f) == map(y)(f compose g).
     *
     * First of all see that, using Given:
     * map(map(y)(id))(f) = map(y)(f)
     *
     * Now using the fact that: f compose id = f
     * map(map(y)(id))(f) = map(y)(f) = map(y)(f compose id)
     *
     * Now substituting g for id, we get:
     * map(map(y)(g))(f) = map(y)(f compose g)
     *
     * QED.
     */

    def delay[A](fa: => Par[A]): Par[A] = es => fa(es)


  }
  val s = Executors.newFixedThreadPool(1000)

  println(sumByTraverse(IndexedSeq(0,1,2,3,4,5))(s))
  println(maxByTraverse(IndexedSeq(0, 4, 8, 12, 3, 20))(s))
  println(countWordsByTraverse(List("aaa bb", "bbbb", "cc dd ff"))(s))
}
