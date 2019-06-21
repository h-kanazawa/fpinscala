package fpinscala.streamingio

object SimpleStreamTransducers {
  sealed trait Process[I, O] {
    def apply(s: Stream[I]): Stream[O] = this match {
      case Halt() => Stream()
      case Await(recv) => s match {
        case h #:: t => recv(Some(h))(t)
        case xs => recv(None)(xs)
      }
      case Emit(h, t) => h #:: t(s)
    }

    def repeat: Process[I, O] = {
      def go(p: Process[I, O]): Process[I, O] = p match {
        case Halt() => go(this)
        case Await(recv) => Await {
          case None => recv(None)
          case i => go(recv(i))
        } // Await.apply
        case Emit(h, t) => Emit(h, go(t)) // Emit.apply(h, go(t))
      }

      go(this)
    }
  }

  case class Emit[I, O](
    head: O,
    tail: Process[I, O] = Halt[I, O]()
  ) extends Process[I, O]

  case class Await[I, O](
    recv: Option[I] => Process[I, O]
  ) extends Process[I, O]

  case class Halt[I, O]() extends Process[I, O]

  def liftOne[I, O](f: I => O): Process[I, O] =
    Await {
      case Some(i) => Emit(f(i))
      case None => Halt()
    }

  def lift[I, O](f: I => O): Process[I, O] =
    liftOne(f).repeat

  def filter[I](f: I => Boolean): Process[I, I] =
    Await[I, I] {
      case Some(i) if f(i) => Emit(i)
      case _ => Halt()
    }.repeat

  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] =
      Await {
        case Some(d) => Emit(d + acc, go(d + acc))
        case None => Halt()
      }
    go(0.0)
  }

  // Exercise 15.1
  def take[I](n: Int): Process[I, I] =
    Await {
      case Some(d) if (n > 0) => Emit(d, take(n - 1))
      case _ => Halt()
    }

  def drop[I](n: Int): Process[I, I] =
    if (n > 0) Await(_ => drop(n - 1))
    else lift[I, I]((x: I) => x)

  def takeWhile[I](f: I => Boolean): Process[I, I] =
    Await {
      case Some(d) if (f(d)) => Emit(d, takeWhile(f))
      case _ => Halt()
    }

  def dropWhile[I](f: I => Boolean): Process[I, I] =
    Await {
      case Some(d) if (f(d)) => dropWhile(f)
      case Some(d) => Emit(d, lift[I, I]((x: I) => x))
      case None => Halt()
    }

  def count[I]: Process[I, Int] = {
    def go(acc: Int): Process[I, Int] =
      Await {
        case Some(_) => Emit(acc + 1, go(acc + 1))
        case _ => Halt()
      }
    go(0)
  }


  def main(args: Array[String]): Unit = {
    val p = liftOne((x: Int) => x * 2)
    val xs = p(Stream(1, 2, 3)).toList
    println(xs)

    val p2 = lift((x: Int) => x * 2)
    val xs2 = p2(Stream(1, 2, 3)).toList
    println(xs2)

    val even = filter((x: Int) => x % 2 == 0)
    val evens = even(Stream(1, 2, 3, 4)).toList
    println(evens)

    val s = sum(Stream(1, 2, 3, 4)).toList
    println(s)

    val a1 = take(4)(Stream(1, 2, 3, 4, 5, 6)).toList
    println(a1)

    val a2 = drop(4)(Stream(1, 2, 3, 4, 5, 6)).toList
    println(a2)

    val a3 = takeWhile[Int](_ > 10)(Stream(11, 12, 13, 4, 5, 6)).toList
    println(a3)

    val a4 = dropWhile[Int](_ > 10)(Stream(11, 12, 13, 4, 5, 6, 7, 8)).toList
    println(a4)

    val a5 = count(Stream("a", "b", "c", "d", "a")).toList
    println(a5)
  }
}