package partTwo.chapterSeven

import partTwo.chapterSeven.ImprovedPar.Par.{parMap, run}

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}
import java.util.concurrent.atomic.AtomicReference


object ImprovedPar extends App {
  sealed trait Future[A] {
    private[chapterSeven] def apply(k: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  object Par {
    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      p(es) { a => ref.set(a); latch.countDown() }
      latch.await()
      ref.get
    }

    def unit[A](a: A): Par[A] = es => new Future[A] {
      def apply(cb: A => Unit): Unit = cb(a)
    }

    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
      }

    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] {
        def call: Unit = r
      })

    def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None
          val combiner = Actor[Either[A, B]](es) {
            case Left(a) => br match {
              case None => ar = Some(a)
              case Some(b) => eval(es)(cb(f(a, b)))
            }
            case Right(b) => ar match {
              case None => br = Some(b)
              case Some(a) => eval(es)(cb(f(a, b)))
            }
          }

          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
        }
      }

    def delay[A](a: => A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    def async[A](f: (A => Unit) => Unit): Par[A] = es => new Future[A] {
      def apply(k: A => Unit): Unit = f(k)
    }

    // specialized version of `map`
    def map[A, B](p: Par[A])(f: A => B): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => eval(es) {
            cb(f(a))
          })
      }

    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
      as match {
        case Nil => unit(Nil)
        case h :: t => map2(h, fork(sequence(t)))(_ :: _)
      }

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
      sequence(ps.map(asyncF(f)))
    }

    def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          p(es) { b =>
            if (b) eval(es) {
              t(es)(cb)
            }
            else eval(es) {
              f(es)(cb)
            }
          }
      }

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      es => choices(run(es)(n))(es)

    def choiceByChoiceN[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      choiceN(map(p) {
        if (_) 1 else 0
      })(List(t, f))

    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      es => choices(run(es)(key))(es)

    def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = { es => choices(run(es)(pa))(es) }

    def choiceByChooser[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      chooser(p) { if (_) t else f }

    def choiceNByChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      chooser(n)(choices)

    def choiceMapByChooser[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      chooser(key)(choices)

    def join[A](a: Par[Par[A]]): Par[A] = { es => run(es)(a)(es) }

    def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = join(map(a)(f))

    def joinByFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(identity)
  }

  val p = parMap(List.range(1, 100000))(math.sqrt(_))
  val x = run(Executors.newFixedThreadPool(2))(p)
  println(s"Showing x ${x}")
}
