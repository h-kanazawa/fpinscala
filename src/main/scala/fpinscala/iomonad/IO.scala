package fpinscala.iomonad

import scala.io.StdIn.readLine
import language.higherKinds
import language.postfixOps

object IO0 {
  trait IO { self =>
    def run: Unit

    def ++(io: IO) = new IO {
      def run = { self.run; io.run }
    }
  }

  object IO {
    def PrintLine(msg: String): IO =
      new IO {
        def run = println(msg)
      }

    def empty: IO = new IO {
      def run = ()
    }
  }

  def main(args: Array[String]): Unit = {
    (IO.empty ++ IO.PrintLine("line 1") ++ IO.PrintLine("Line 2")).run
  }
}

object IO1 {
  sealed trait IO[A] { self =>
    def run: A

    def map[B](f: A => B): IO[B] =
      new IO[B] {
        def run = f(self.run)
      }

    def flatMap[B](f: A => IO[B]): IO[B] =
      new IO[B] {
        def run = f(self.run).run
      }
  }

  object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] { def run = a }

    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = fa flatMap f

    def apply[A](a: => A): IO[A] = unit(a)

    def ref[A](a: A): IO[IORef[A]] = IO { new IORef(a) }

    sealed class IORef[A](var value: A) {
      def set(a: A): IO[A] = IO { value = a; a }
      def get: IO[A] = IO { value }
      def modify(f: A => A): IO[A] = get flatMap (a => set(f(a)))
    }
  }

  def ReadLine: IO[String] = IO { readLine }

  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine((d + 3).toString)
  } yield ()

  val echo = ReadLine.flatMap(PrintLine)

  val readInt: IO[Int] = ReadLine.map(_.toInt)

  val readInts: IO[(Int,Int)] = readInt ** readInt

  import IO._

  val helpstring = """
                     | The Amazing Factorial REPL, v2.0
                     | q - quit
                     | <number> - compute the factorial of the given number
                     | <anything else> - bomb with horrible error
                   """.trim.stripMargin

  def factorial(n: Int): IO[Int] = for {
    acc <- ref(1)
    _ <- foreachM (1 to n toStream) (i => acc.modify(_ * i).skip)
    result <- acc.get
  } yield result

  val factorialREPL: IO[Unit] = sequence_(
    IO { println(helpstring) },
    doWhile { IO { readLine } } { line =>
      val ok = line != "q"
      when (ok) { for {
        n <- factorial(line.toInt)
        _ <- IO { println("factorial: " + n) }
      } yield () }
    }
  )

  def main(args: Array[String]): Unit = {
    IO.forever(PrintLine("Still going ...")).run
  }
}

object IO2a {
  sealed trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] =
      FlatMap(this, f)
    def map[B](f: A => B): IO[B] =
      flatMap(f andThen (Return(_)))
  }
  case class Return[A](a: A) extends IO[A]
  case class Suspend[A](resume: () => A) extends IO[A]
  case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

  object IO extends Monad[IO] { // Notice that none of these operations DO anything
    def unit[A](a: => A): IO[A] = Return(a)
    def flatMap[A,B](a: IO[A])(f: A => IO[B]): IO[B] = a flatMap f
    def suspend[A](a: => IO[A]) =
      Suspend(() => ()).flatMap { _ => a }
  }

  def printLine(s: String): IO[Unit] =
    Suspend(() => Return(println(s)))

  val p = IO.forever(printLine("Still going..."))

  val actions: Stream[IO[Unit]] =
    Stream.fill(100000)(printLine("Still going..."))
  val composite: IO[Unit] =
    actions.foldLeft(IO.unit(())) { (acc, a) => acc flatMap { _ => a } }

//  @annotation.tailrec
//  def run[A](io: IO[A]): A = io match {
//    case Return(a) => a
//    case Suspend(r) => r()
//    case FlatMap(io, f) => io match {
//      case Return(a) => run[A](f(a))
//      case Suspend(r) => run[A](f(r()))
//      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f)
//    }
//  }
}

object IO2b {
  sealed trait TailRec[A] {
    def flatMap[B](f: A => TailRec[B]): TailRec[B] =
      FlatMap(this, f)
    def map[B](f: A => B): TailRec[B] =
      flatMap(f andThen (Return(_)))
  }
  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A,B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

  object TailRec extends Monad[TailRec] {
    def unit[A](a: => A): TailRec[A] = Return(a)
    def flatMap[A,B](a: TailRec[A])(f: A => TailRec[B]): TailRec[B] = a flatMap f
    def suspend[A](a: => TailRec[A]) =
      Suspend(() => ()).flatMap { _ => a }

  }

//  @annotation.tailrec def run[A](t: TailRec[A]): A = t match {
//    case Return(a) => a
//    case Suspend(r) => r()
//    case FlatMap(x, f) => x match {
//      case Return(a) => run(f(a))
//      case Suspend(r) => run(f(r()))
//      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
//    }
//  }
}

object IO2c {
  import fpinscala.parallelism.Nonblocking._

  sealed trait Async[A] {
    def flatMap[B](f: A => Async[B]): Async[B] =
      FlatMap(this, f)
    def map[B](f: A => B): Async[B] =
      flatMap(f andThen (Return(_)))
  }
  case class Return[A](a: A) extends Async[A]
  case class Suspend[A](resume: Par[A]) extends Async[A]
  case class FlatMap[A,B](sub: Async[A], k: A => Async[B]) extends Async[B]

  object Async extends Monad[Async] {
    def unit[A](a: => A): Async[A] = Return(a)
    def flatMap[A,B](a: Async[A])(f: A => Async[B]): Async[B] = a flatMap f
  }

//  @annotation.tailrec def step[A](async: Async[A]): Async[A] = async match {
//    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
//    case FlatMap(Return(x), f) => step(f(x))
//    case _ => async
//  }
//
//  def run[A](async: Async[A]): Par[A] = step(async) match {
//    case Return(a) => Par.unit(a)
//    case Suspend(r) => r
//    case FlatMap(x, f) => x match {
//      case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
//      case _ => sys.error("Impossible, since `step` eliminates these cases")
//    }
//  }
}

object IO3 {

  /*
  We can generalize `TailRec` and `Async` to the type `Free`, which is
  a `Monad` for any choice of `F`.
  */

  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] =
      FlatMap(this, f)

    def map[B](f: A => B): Free[F, B] =
      flatMap(f andThen (Return(_)))
  }

  case class Return[F[_], A](a: A) extends Free[F, A]

  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]

  case class FlatMap[F[_], A, B](s: Free[F, A],
                                 f: A => Free[F, B]) extends Free[F, B]

  // Exercise 1: Implement the free monad
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] =
    new Monad[({type f[a] = Free[F, a]})#f] {
      def unit[A](a: => A) = Return(a)

      def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]) = fa flatMap f
    }

  // Exercise 2: Implement a specialized `Function0` interpreter.
//  @annotation.tailrec
//  def runTrampoline[A](a: Free[Function0, A]): A = (a) match {
//    case Return(a) => a
//    case Suspend(r) => r()
//    case FlatMap(x, f) => x match {
//      case Return(a) => runTrampoline {
//        f(a)
//      }
//      case Suspend(r) => runTrampoline {
//        f(r())
//      }
//      case FlatMap(a0, g) => runTrampoline {
//        a0 flatMap { a0 => g(a0) flatMap f }
//      }
//    }
//  }

  // Exercise 3: Implement a `Free` interpreter which works for any `Monad`
//  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = step(a) match {
//    case Return(a) => F.unit(a)
//    case Suspend(r) => r
//    case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
//    case _ => sys.error("Impossible, since `step` eliminates these cases")
//  }

  // return either a `Suspend`, a `Return`, or a right-associated `FlatMap`
//  @annotation.tailrec
//  def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
//    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
//    case FlatMap(Return(x), f) => step(f(x))
//    case _ => a
//  }
}