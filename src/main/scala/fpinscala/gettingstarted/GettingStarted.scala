package fpinscala.gettingstarted

object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(n, factorial(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  // 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(a: Int, b: Int, n: Int): Int =
      if (n == 1) a
      else go(b, a+b, n-1)

    go(1, 1, n)
   }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n+1)

    loop(0)
  }

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length-1) true
      else if (!ordered(as(n), as(n+1))) false
      else loop(n+1)

    loop(0)
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatResult("absolute value", -42, abs))
    println(formatFactorial(7))
    println(formatResult("factorial", 7, factorial))
    println(fib(10))
    println(findFirst(Array("ab", "cd", "ef"), (a: String) => a == "cd"))
    println(findFirst(Array("ab", "cd", "ef"), (a: String) => a == "xy"))
    println(isSorted(Array(1,2,5,7,8,12,23), (a: Int, b: Int) => a < b))
    println(isSorted(Array(1,2,5,7,8,12,23), (a: Int, b: Int) => a > b))
  }
}
