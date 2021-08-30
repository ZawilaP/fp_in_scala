package partOne.chapterSix

import partOne.chapterSix.State.State.{unit, sequence, modify, get}

import scala.annotation.tailrec

object State extends App {
  case class State[S, +A](run: S => (A,S)) {
    def map[B](f: A => B): State[S, B] = {
       flatMap(a => unit(f(a)))
    }
    def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] = {
      flatMap(a => rb map (b => f(a, b)))
    }
    def flatMap[B](g: A => State[S, B]): State[S, B] = State(s => {
      val (a, s1) = run(s)
      g(a).run(s1)
    })
  }

  object State {
    def unit[S,A](a: A): State[S, A] = State(s => (a,s))
    def sequence[A, S](fs: List[State[S, A]]): State[S, List[A]] = {
      fs.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))
    }
    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    } yield ()
    def get[S]: State[S, S] = State(s => (s, s))
    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  }

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int) {
    private def update(i: Input): Machine =
      (i, locked, candies, coins) match {
        case (_, a, b, c) if b < 1 => Machine(a, b, c)
        case (Coin, true, a, b) if a > 0 => Machine(false, a, b + 1)
        case (Turn, false, a, b) => Machine(true, a - 1, b)
        case (Coin, false, a, b) => Machine(false, a, b)
        case (Turn, true, a, b) => Machine(true, a, b)
      }

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
      @tailrec
      def helper(inputs: List[Input], acc: Machine): Machine = {
        inputs.length match {
          case 0 => acc
          case _ => helper(inputs.tail, acc.update(inputs.head))
        }
      }

      val m = helper(inputs, this)
      unit((m.coins, m.candies))
    }
  }

  println(Machine(true, 10, 10).simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)))
}
