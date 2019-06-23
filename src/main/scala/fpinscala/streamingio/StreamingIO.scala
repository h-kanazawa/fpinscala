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

    def |>[O2](p2: Process[O, O2]): Process[I, O2] =
      p2 match {
        case Halt() => Halt()
        case Emit(h, t) => Emit(h, this |> t)
        case Await(f) => this match {
          case Halt() => Halt() |> f(None)
          case Emit(h, t) => t |> f(Some(h))
          case Await(g) => Await((i: Option[I]) => g(i) |> p2)
        }
      }

    def map[O2](f: O => O2): Process[I, O2] = this |> lift(f)

    def ++(p: => Process[I, O]): Process[I, O] = this match {
      case Halt() => p
      case Emit(h, t) => Emit(h, t ++ p)
      case Await(recv) => Await(recv andThen (_ ++ p))
    }

    def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] =
      this match {
        case Halt() => Halt()
        case Emit(h, t) => f(h) ++ t.flatMap(f)
        case Await(recv) => Await(recv andThen (_ flatMap f))
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

  // Exercise 15.2
  def count[I]: Process[I, Int] = {
    def go(acc: Int): Process[I, Int] =
      Await {
        case Some(_) => Emit(acc + 1, go(acc + 1))
        case _ => Halt()
      }
    go(0)
  }

  // Exercise 15.3
  def mean: Process[Double, Double] = {
    def go(sum: Double, count: Double): Process[Double, Double] =
      Await {
        case Some(d) => Emit((sum + d) / (count + 1), go(sum + d, count + 1))
        case _ => Halt()
      }
    go(0.0, 0.0)
  }

  val mean2: Process[Double, Double] =
    (zip[Double, Double, Int](sum, count)) |> lift[(Double, Int), Double]{
      case (s, n) => s / n
    }

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
    Await {
      case Some(d) => {
        val (o, s2) = f(d, z)
        Emit(o, loop(s2)(f))
      }
      case _ => Halt()
    }

  // Exercise 15.4
  def sumViaLoop: Process[Double, Double] =
    loop(0.0)((i, s) => (i + s, i + s))

  def countViaLoop[I]: Process[I, Int] =
    loop(0)((_, s) => (s + 1, s + 1))

  def feed[A, B](oa: Option[A])(p: Process[A, B]): Process[A, B] =
    p match {
      case Halt() => p
      case Emit(h, t) => Emit(h, feed(oa)(t))
      case Await(recv) => recv(oa)
    }

  def zip[A, B, C](p1: Process[A, B], p2: Process[A, C]): Process[A, (B, C)] =
    (p1, p2) match {
      case (Halt(), _) => Halt()
      case (_, Halt()) => Halt()
      case (Emit(b, t1), Emit(c, t2)) => Emit((b, c), zip(t1, t2))
      case (Await(recv1), _) =>
        Await((oa: Option[A]) => zip(recv1(oa), feed(oa)(p2)))
      case (_, Await(recv2)) =>
        Await((oa: Option[A]) => zip(feed(oa)(p1), recv2(oa)))
    }

  def any: Process[Boolean, Boolean] =
    loop(false)((b: Boolean, s) => (s || b, s || b))

  def exists[I](f: I => Boolean): Process[I, Boolean] =
    lift(f) |> any


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

    val sl = sumViaLoop(Stream(1, 2, 3, 4)).toList
    println(sl)

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

    val a5l = countViaLoop(Stream("a", "b", "c", "d", "a")).toList
    println(a5l)

    println("mean")
    val a6 = mean(Stream(7, 3, 5, 1, 10, 3)).toList
    println(a6)

    val a6b = mean2(Stream(7, 3, 5, 1, 10, 3)).toList
    println(a6b)

    val a7 = (filter[Int](_ % 2 == 0) |> lift(_ + 1))(Stream(7, 3, 6, 1, 10, 3, -2, 0, 11)).toList
    println(a7)

    println("exists")
    val a8a = exists[Int](_ > 10)(Stream(9, 10, 1, 4, 6)).toList
    println(a8a)

    val a8b = exists[Int](_ >= 10)(Stream(9, 10, 1, 4, 6)).toList
    println(a8b)
  }
}

