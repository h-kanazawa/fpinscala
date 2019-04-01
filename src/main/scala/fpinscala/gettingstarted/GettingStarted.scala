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

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatResult("absolute value", -42, abs))
    println(formatFactorial(7))
    println(formatResult("factorial", 7, factorial))
    println(fib(10))
  }
}
