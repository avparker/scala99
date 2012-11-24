package scala99

object Lists {

  /*
   *  P01 (*) Find the last element of a list.
   *  Example:
   *  scala> last(List(1, 1, 2, 3, 5, 8))
   *  res0: Int = 8
   */
  def last[A](ls: List[A]): A = {
    // throw new RuntimeException("Not implemented yet")
    
	// implemented without using the built-in method on List
    ls match {
      case Nil => throw new NoSuchElementException
      case x :: Nil => x
      case x :: xs => last(xs)
    }
    
    // just for fun, an implementation that uses foldLeft
//    if (ls.isEmpty) throw new NoSuchElementException
//    else ls.foldLeft(ls.head)( (x, y) => y )
  }

  /*
   * P02 (*) Find the last but one element of a list.
   * Example:
   * scala> penultimate(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 5
   */
  def penultimate[A](ls: List[A]): A = {
    // throw new RuntimeException("Not implemented yet")
    ls match {
      case x :: y :: Nil => x
      case x :: xs => penultimate(xs) // this list has 1 or >2 entries
      case _ => throw new NoSuchElementException
    }
  }

  /*
   * P03 (*) Find the Nth element of a list.
   * By convention, the first element in the list is element 0.
   * Example:
   * scala> nth(2, List(1, 1, 2, 3, 5, 8))
   * res0: Int = 2
   */
  def nth[A](n: Int, ls: List[A]): A = {
    // throw new RuntimeException("Not implemented yet")
    ls match {
      case Nil => throw new NoSuchElementException
      case x :: xs =>
        if (n == 0) x
        else nth(n - 1, xs)
    }
  }

  /*
   * P04 (*) Find the number of elements of a list.
   * Example:
   * scala> length(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 6
   */
  def length[A](ls: List[A]): Int = {
    //throw new RuntimeException("Not implemented yet")
    
//	// implement with a tail-recursive helper function
//    def length0(n: Int, ls: List[A]): Int = {
//      ls match {
//        case Nil => n
//        case _ => length0(n+1, ls.tail)
//      }
//    }
//    length0(0, ls)
    
    // start with 0, add one for each element in the list
    ls.foldLeft(0)( (x, _) => x + 1 )
  }

  /*
   * P05 (*) Reverse a list.
   * Example:
   * scala> reverse(List(1, 1, 2, 3, 5, 8))
   * res0: List[Int] = List(8, 5, 3, 2, 1, 1)
   */
  def reverse[A](ls: List[A]): List[A] = {
    throw new RuntimeException("Not implemented yet")
  }

  def flatten(ls: List[Any]): List[Any] = {
    throw new RuntimeException("Not implemented yet")
  }

  def compress[A](ls: List[A]): List[A] = {
    throw new RuntimeException("Not implemented yet")
  }

  def pack[A](ls: List[A]): List[List[A]] = {
    throw new RuntimeException("Not implemented yet")
  }

  def encode[A](ls: List[A]): List[(Int, A)] = {
    throw new RuntimeException("Not implemented yet")
  }

  def encodeModified[A](ls: List[A]): List[Any] = {
    throw new RuntimeException("Not implemented yet")
  }

  def decode[A](ls: List[(Int, A)]): List[A] = {
    throw new RuntimeException("Not implemented yet")
  }

  def encodeDirect[A](ls: List[A]): List[Any] = {
    throw new RuntimeException("Not implemented yet")
  }

  def duplicate[A](ls: List[A]): List[A] = {
    throw new RuntimeException("Not implemented yet")
  }

  def duplicateN[A](n: Int, ls: List[A]): List[A] = {
    throw new RuntimeException("Not implemented yet")
  }

  def drop[A](n: Int, ls: List[A]): List[A] = {
    throw new RuntimeException("Not implemented yet")
  }

}
