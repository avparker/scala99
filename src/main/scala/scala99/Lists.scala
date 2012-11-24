package scala99

object Lists {

  def last[A](ls: List[A]): A = ls match {
    case Nil => throw new NoSuchElementException
    case x :: Nil => x
    case x :: xs => last(xs)
  }

  def penultimate[A](ls: List[A]) : A = ls match {
    case x :: y :: Nil => x
    case x :: xs => penultimate(xs)
    case _ => throw new NoSuchElementException
  }

}
