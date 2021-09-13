package partOne.misc

import scala.annotation.tailrec

object SecondToMax extends App {
  def secondToMax(xs: List[Int]): Option[Int] = {
    @tailrec
    def helper(xs: List[Int], biggest: Int, second: Int): Option[Int] = {
      xs.length match {
        case 0 => Some(second)
        case _ =>
          val current = xs.head
          current match {
            case x if x >= biggest => helper(xs.tail, x, biggest)
            case x if x >= second => helper(xs.tail, biggest, x)
            case _ => helper(xs.tail, biggest, second)
          }
      }
    }

    helper(xs, 0, 0)
  }

  trait Sortable[T] {
    def >=(a: T, b: T): Boolean

    val neutralEl: T
  }

  implicit val intSortable: Sortable[Int] = new Sortable[Int] {
    def >=(a: Int, b: Int): Boolean = a >= b

    override val neutralEl: Int = 0
  }

  implicit val strSortable: Sortable[String] = new Sortable[String] {
    def >=(a: String, b: String): Boolean = a >= b

    override val neutralEl: String = ""
  }

  def polymorphicSecondToMax[T](xs: List[T])(implicit sortable: Sortable[T]): Option[T] = {
    @tailrec
    def helper(xs: List[T], biggest: T, second: T): Option[T] = {
      xs.length match {
        case 0 => Some(second)
        case _ =>
          val current = xs.head
          current match {
            case x if sortable.>=(x, biggest) => helper(xs.tail, x, biggest)
            case x if sortable.>=(x, second) => helper(xs.tail, biggest, x)
            case _ => helper(xs.tail, biggest, second)
          }
      }
    }

    xs.length match {
      case 0 => None
      case _ => helper(xs, sortable.neutralEl, sortable.neutralEl)
    }
  }

  val sampleList = List(0, 4, 3, 5, 8)

  println(polymorphicSecondToMax(sampleList))
}
