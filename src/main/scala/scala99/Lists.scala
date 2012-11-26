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
  // NOTE: take from the solution on the original problem page
  //       I didn't figure this one out :-(
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
    //    // helper to create a list of n copies of a
    //    def makeList(n: Int, a: A, accum: List[A]): List[A] =
    //      if (n == 0) accum
    //      else makeList(n - 1, a, a :: accum)
    //    ls flatMap {
    //      case (n, a) => makeList(n, a, Nil)
    //    }

    // List.fill(n) returns a function which creates a list of n items
    ls flatMap { case (n, a) => List.fill(n)(a) }
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

  /*
   * P14 (*) Duplicate the elements of a list.
   * 
   * Example:
   * scala> duplicate(List('a, 'b, 'c, 'c, 'd))
   * res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
   */
  def duplicate[A](ls: List[A]): List[A] =
    //    ls.foldLeft(List[A]())( (a, l) => l :: (l :: a) ).reverse
    ls flatMap { a => List(a, a) }

  /*
   * P15 (**) Duplicate the elements of a list a given number of times.
   * 
   * Example:
   * scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
   * res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
   */
  def duplicateN[A](n: Int, ls: List[A]): List[A] =
    //  {
    //    def makeList(m: Int, a: A, accum: List[A]): List[A] =
    //      if (m == 0) accum
    //      else makeList(m - 1, a, a :: accum)
    //    ls flatMap {
    //      a => makeList(n, a, Nil)
    //    }
    //  }
    //
    // List.fill(n) returns a function which creates a list of n items
    ls flatMap { a => List.fill(n)(a) }

  /*
   * P16 (**) Drop every Nth element from a list.
   * 
   * Example:
   * scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
   */
  def drop[A](n: Int, ls: List[A]): List[A] = {
    //    def dropIfN(m: Int, ls: List[A], accum: List[A]): List[A] = ls match {
    //      case Nil => accum.reverse
    //      case _ =>
    //    	if (m % n == 0) dropIfN(m+1, ls.tail, accum)
    //    	else dropIfN(m+1, ls.tail, ls.head :: accum)
    //    }
    //    dropIfN(1, ls, Nil)

    // matching on tuples cleans up the code a little
    // I just learned this trick from the solution on the original problem page
    def drop0(c: Int, ls: List[A], accum: List[A]): List[A] = (c, ls) match {
      case (_, Nil) => accum.reverse
      case (1, x :: xs) => drop0(n, xs, accum)
      case (_, x :: xs) => drop0(c - 1, xs, x :: accum)
    }
    drop0(n, ls, Nil)
  }

  /*
   * P17 (*) Split a list into two parts.
   * The length of the first part is given. Use a Tuple for your result.
   * 
   * Example:
   * scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   */
  def split[A](n: Int, ls: List[A]): (List[A], List[A]) =
    (ls.take(n), ls.drop(n))

  /*
   * P18 (**) Extract a slice from a list.
   * Given two indices, I and K, the slice is the list containing the elements from and 
   * including the Ith element up to but not including the Kth element of the original list.
   * Start counting the elements with 0.
   * 
   * Example:
   * scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: List[Symbol] = List('d, 'e, 'f, 'g)
   */
  def slice[A](n: Int, m: Int, ls: List[A]): List[A] =
    ls.drop(n).take(m - n)

  /*
   * P19 (**) Rotate a list N places to the left.
   * 
   * Examples:
   * scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
   * scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
   */
  def rotate[A](n: Int, ls: List[A]): List[A] = {
    //    // this performs badly for negative values
    //    if (n >= 0) (ls drop n) ::: (ls take n)
    //    else (ls takeRight -n) ::: (ls dropRight -n)

    // no point rotating by larger than the length, it just repeats
    // negative values are shifted into the positive equivalent by the extra edition and mod
    val length = ls.length
    val m = ((n % length) + length) % length
    (ls drop m) ::: (ls take m)
  }

  /*
   * P20 (*) Remove the Kth element from a list.
   * Return the list and the removed element in a Tuple.
   * Elements are numbered from 0.
   * 
   * Example:
   * scala> removeAt(1, List('a, 'b, 'c, 'd))
   * res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
   */
  def removeAt[A](n: Int, ls: List[A]): (List[A], A) = {
    //    val pre = ls.take(n)
    //    val rest = ls.drop(n)
    //	  val (pre, rest) = split(n, ls) 
    val (pre, rest) = ls.splitAt(n)
    if (rest.isEmpty) throw new NoSuchElementException
    else (pre ::: rest.tail, rest.head)
  }

  /*
   * P21 (*) Insert an element at a given position into a list.
   * 
   * Example:
   * scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
   * res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
   */
  def insertAt[A](a: A, n: Int, ls: List[A]): List[A] = {
    val (pre, post) = ls.splitAt(n)
    pre ::: a :: post
  }

  /*
   * P22 (*) Create a list containing all integers within a given range.
   * 
   * Example:
   * scala> range(4, 9)
   * res0: List[Int] = List(4, 5, 6, 7, 8, 9)
   */
  def range(n: Int, m: Int): List[Int] = {
    // use built-in Range class (lazy) and force to a list
//	Range(n, m).toList
    
    // simple recursive
//    if (n <= m) n :: range(n+1, m)
//    else Nil
    
    // tail-recursive solution - build the list in reverse
    def range0(a: Int, accum: List[Int]) : List[Int] =
      if (a < n) accum
      else range0(a-1, a :: accum)
    range0(m, Nil)
  }

  /*
   * P23 (**) Extract a given number of randomly selected elements from a list.
   * Hint: Use the solution to problem P20
   * 
   * Example:
   * scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
   * res0: List[Symbol] = List('e, 'd, 'a)
   */
  def randomSelect[A](n: Int, ls: List[A]): List[A] =
    if (n <= 0) Nil
    else {
      val random = util.Random.nextInt(n);
      val (remaining, l) = removeAt(random, ls)
      l :: randomSelect(n-1, remaining)
    }

  /*
   * P24 (*) Lotto: Draw N different random numbers from the set 1..M.
   * 
   * Example:
   * scala> lotto(6, 49)
   * res0: List[Int] = List(23, 1, 17, 33, 21, 37)
   */
  def lotto(n: Int, m: Int): List[Int] =
    randomSelect(n, range(1, m))

  /*
   * P25 (*) Generate a random permutation of the elements of a list.
   * Hint: Use the solution of problem P23.
   * 
   * Example:
   * scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
   * res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
   */
  def randomPermute[A](ls: List[A]): List[A] =
    randomSelect(ls.length, ls)

  /*
   * P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
   * In how many ways can a committee of 3 be chosen from a group of 12 people?
   * We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient).
   * For pure mathematicians, this result may be great.
   * But we want to really generate all the possibilities.
   * 
   * Example:
   * scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
   * res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ... 
   */
  def combinations[A](n: Int, ls: List[A]): List[List[A]] =
    if (n <= 0) List(Nil)
    else {
      for {
        i <- range(0, ls.length-1)
        rest <- combinations(n-1, ls.drop(i+1)) // dropping ensures we don't generate duplicates
      } yield ls(i) :: rest
    }

  /*
   * P27 (**) Group the elements of a set into disjoint subsets.
   * a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons?
   * Write a function that generates all the possibilities.
   * 
   * Example:
   * scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
   * res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...
   * 
   * b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
   * Example:
   * 
   * scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
   * res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...
   * 
   * Note that we do not want permutations of the group members;
   * i.e. ((Aldo, Beat), ...) is the same solution as ((Beat, Aldo), ...).
   * However, we make a difference between ((Aldo, Beat), (Carla, David), ...) and ((Carla, David), (Aldo, Beat), ...).
   * 
   * You may find more about this combinatorial problem in a good book on discrete mathematics
   * under the term "multinomial coefficients".
   */
  def group3[A](ls: List[A]): List[List[List[A]]] = ???
  def group[A](sizes: List[Int], ls: List[A]): List[List[List[A]]] = ???

  /*
   * P28 (**) Sorting a list of lists according to length of sublists.
   * a) We suppose that a list contains elements that are lists themselves.
   * The objective is to sort the elements of the list according to their length.
   * E.g. short lists first, longer lists later, or vice versa.
   * 
   * Example:
   * scala> lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
   * res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
   * 
   * b) Again, we suppose that a list contains elements that are lists themselves.
   * But this time the objective is to sort the elements according to their length frequency;
   * i.e. in the default, sorting is done ascendingly, lists with rare lengths are placed,
   * others with a more frequent length come later.
   * 
   * Example:
   * scala> lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
   * res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
   * 
   * Note that in the above example, the first two lists in the result have length 4 and 1
   * and both lengths appear just once. The third and fourth lists have length 3 and there
   * are two list of this length. Finally, the last three lists have length 2. This is the most frequent length.
   */
  def lsort[A](lls: List[List[A]]): List[List[A]] = ???
  def lsortFreq[A](lls: List[List[A]]): List[List[A]] = ???

  def ??? : Nothing = throw new Error("This problem has not been implemented yet.")
}
