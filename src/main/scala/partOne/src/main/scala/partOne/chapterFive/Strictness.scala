package partOne.chapterFive

object Strictness extends App {
  def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A = {
    if (cond) onTrue() else onFalse()
  }
}
