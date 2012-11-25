package scala99

object Lists {

  /*
   *  P01 (*) Find the last element of a list.
   *  
   *  Example:
   *  scala> last(List(1, 1, 2, 3, 5, 8))
   *  res0: Int = 8
   */
  def last[A](ls: List[A]): A = {
    // tail-recursive solution
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
   * 
   * Example:
   * scala> penultimate(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 5
   */
  def penultimate[A](ls: List[A]): A = {
    // tail-recursive solution
    ls match {
      case x :: y :: Nil => x
      case x :: xs => penultimate(xs) // this list has 1 or >2 entries
      case _ => throw new NoSuchElementException
    }
  }

  /*
   * P03 (*) Find the Nth element of a list.
   * By convention, the first element in the list is element 0.
   * 
   * Example:
   * scala> nth(2, List(1, 1, 2, 3, 5, 8))
   * res0: Int = 2
   */
  def nth[A](n: Int, ls: List[A]): A = {
    // tail-recursive solution
    ls match {
      case Nil => throw new NoSuchElementException
      case x :: xs =>
        if (n == 0) x
        else nth(n - 1, xs)
    }
  }

  /*
   * P04 (*) Find the number of elements of a list.
   * 
   * Example:
   * scala> length(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 6
   */
  def length[A](ls: List[A]): Int = {
    // tail-recursive solution
    //    def length0(n: Int, ls: List[A]): Int = {
    //      ls match {
    //        case Nil => n
    //        case _ => length0(n+1, ls.tail)
    //      }
    //    }
    //    length0(0, ls)

    // the tail-recursive solution transformed to a foldLeft
    // start with 0, add one for each element in the list
    ls.foldLeft(0)((x, _) => x + 1)
  }

  /*
   * P05 (*) Reverse a list.
   * 
   * Example:
   * scala> reverse(List(1, 1, 2, 3, 5, 8))
   * res0: List[Int] = List(8, 5, 3, 2, 1, 1)
   */
  def reverse[A](ls: List[A]): List[A] = {
    // tail-recursive solution
    //    def reverse0(accum: List[A], ls: List[A]): List[A] = ls match {
    //      case Nil => accum
    //      case x :: xs => reverse0(x :: accum, xs)
    //    }
    //    reverse0(Nil, ls)

    // the tail-recursive solution transformed to a foldLeft
    // start with empty list, add each element to the front
    ls.foldLeft(List[A]())((accum, x) => x :: accum)
  }

  /*
   * P06 (*) Find out whether a list is a palindrome.
   * 
   * Example:
   * scala> isPalindrome(List(1, 2, 3, 2, 1))
   * res0: Boolean = true
   */
  def isPalindrome[A](ls: List[A]): Boolean = {
    ls == reverse(ls)
  }

  /*
   * P07 (**) Flatten a nested list structure.
   * 
   * Example:
   * scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
   * res0: List[Any] = List(1, 1, 2, 3, 5, 8)
   */
  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case xs: List[_] => flatten(xs)
    case x => List(x)
  }

  /*
   * P08 (**) Eliminate consecutive duplicates of list elements.
   * If a list contains repeated elements they should be replaced with a single copy of the element.
   * The order of the elements should not be changed.
   * 
   * Example:
   * scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
   */
  def compress[A](ls: List[A]): List[A] = {
    //    // tail-recursive solution
    //    def compress0(accum: List[A], ls: List[A]): List[A] = ls match {
    //      case Nil => accum
    //      case x :: xs => if (!accum.isEmpty && x == accum.head) compress0(accum, xs)
    //      				  else compress0(x :: accum, xs)
    //    }
    //    compress0(Nil, ls).reverse

    // the tail-recursive solution transformed to a foldLeft
    // start with empty list, add each element to the front if it is not already there, then reverse the end result
    ls.foldLeft(List[A]())((a, x) => if (!a.isEmpty && x == a.head) a else x :: a).reverse
  }

  /*
   * P09 (**) Pack consecutive duplicates of list elements into sublists.
   * If a list contains repeated elements they should be placed in separate sublists.
   * 
   * Example:
   * scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
   */
  def pack[A](ls: List[A]): List[List[A]] =
    // this is a bit hard to understand, there's probably a cleaner way
    ls.foldLeft(List[List[A]]()) {
      (a, x) =>
        if (!a.isEmpty && x == a.head.head) ((x :: a.head) :: a.tail) // add it to the existing head list
        else List(x) :: a // start a new list
    }.reverse

  /*
   * P10 (*) Run-length encoding of a list.
   * Use the result of problem P09 to implement the so-called run-length encoding data compression method.
   * Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
   * 
   * Example:
   * scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   */
  def encode[A](ls: List[A]): List[(Int, A)] =
    // this one is nice!
    pack(ls) map (l => (l.length, l.head))

  /*
   * P11 (*) Modified run-length encoding.
   * Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list.
   * Only elements with duplicates are transferred as (N, E) terms.
   * 
   * Example:
   * scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
   */
  def encodeModified[A](ls: List[A]): List[Any] =
    // Doesn't work if you expand the tuple definition, not sure why
    // encode(ls) map ((n, e) => if (n == 1) e else (n, e))
    encode(ls) map (t => if (t._1 == 1) t._2 else t)

  /*
     * P12 (**) Decode a run-length encoded list.
     * Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
     * 
     * Example:
     * scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
     * res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
     */
  def decode[A](ls: List[(Int, A)]): List[A] = {
    // helper to create a list of n copies of a
    def makeList(n: Int, a: A, accum: List[A]): List[A] =
      if (n == 0) accum
      else makeList(n - 1, a, a :: accum)
    ls flatMap {
      case (n, a) => makeList(n, a, List())
    }
  }

  /* 
   * P13 (**) Run-length encoding of a list (direct solution).
   * Implement the so-called run-length encoding data compression method directly.
   * I.e. don't use other methods you've written (like P09's pack); do all the work directly.
   * 
   * Example:
   * scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   */
  def encodeDirect[A](ls: List[A]): List[(Int, A)] =
    // based directly on the P09 implementation
    // this is a bit hard to understand, there's probably a cleaner way
    ls.foldLeft(List[(Int, A)]()) {
      (a, x) =>
        if (!a.isEmpty && x == a.head._2) (a.head._1 + 1, x) :: a.tail // add it to the count in existing head
        else (1, x) :: a // start a new tuple
    }.reverse

  def duplicate[A](ls: List[A]): List[A] = ???

  def duplicateN[A](n: Int, ls: List[A]): List[A] = ???

  def drop[A](n: Int, ls: List[A]): List[A] = ???

  def split[A](n: Int, ls: List[A]): List[A] = ???

  def slice[A](n: Int, m: Int, ls: List[A]): List[A] = ???

  def rotate[A](n: Int, ls: List[A]): List[A] = ???

  def removeAt[A](n: Int, ls: List[A]): List[A] = ???

  def insertAt[A](a: A, n: Int, ls: List[A]): List[A] = ???

  def range(n: Int, m: Int): List[Int] = ???

  def combinations[A](n: Int, ls: List[A]): List[A] = ???

  def lsort[A](lls: List[List[A]]): List[List[A]] = ???

  def ??? : Nothing = throw new Error("Not implemented yet.")
}
