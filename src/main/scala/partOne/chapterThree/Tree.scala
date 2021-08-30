package partOne.chapterThree

object Tree extends App {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(a) => a
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = {
    t match {
      case Leaf(a) => f(a)
      case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

  def sizeViaFold[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)(1 + _ + _)
  }

  def maximumViaFold(t: Tree[Int]): Int = {
    fold(t)(a => a)(_ max _)
  }

  def depthByFold(t: Tree[Int]): Int = {
    fold(t)(_ => 0)((a, b) => 1 + (a max b))
  }

  def leaf[A](a: A): Tree[A] = Leaf(a)
  def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)

  def mapByFold[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(a => leaf(f(a)))(branch)
  }
}
