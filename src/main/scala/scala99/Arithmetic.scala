package scala99

class S99Int(val start: Int) {
  import S99Int._

  def isPrime: Boolean = ???

  def isCoprimeTo(i: Int): Boolean = ???

  def totient: Int = ???

  def primeFactors: List[Int] = ???
  
  def primeFactorMultiplicity: List[(Int, Int)] = ???

  def phi: Int = ???

  def goldbach: (Int, Int) = ???
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

  def gcd(i: Int, j: Int): Int = ???

  def listPrimesinRange(r: Range): List[Int] = ???

  def printGoldbachList(r: Range): Unit = ???

  def printGoldbachListLimited(r: Range, i: Int): Unit = ???

  def ??? : Nothing = throw new Error("This problem has not been implemented yet.")

}
