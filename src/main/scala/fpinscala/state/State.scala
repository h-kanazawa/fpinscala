package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    // Long is 64 bit integer
    //
    // Linear congruential generators, LCG
    // is a Pseudorandom Number Generator
    //
    // Xn+1 = (a * Xn + c) mod m
    // https://en.wikipedia.org/wiki/Linear_congruential_generator
    //
    // 5DEECE66D(16) = 25214903917(10)
    // B(16) = 11(10)
    // FFFFFFFFFFFF(16) = 281474976710655(10) = 2^48 - 1
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)

    // >>> is 0 padding right binary shift
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  // Exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  // Exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  // Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  // Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count2: Int, rng2: RNG, l: List[Int]): (List[Int], RNG) = {
      if (count2 > 0) {
        val (i, r) = nonNegativeInt(rng2)
        go(count2 - 1, r, i :: l)
      }
      else (l, rng2)
    }

    go(count, rng, List[Int]())
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // Exercise 6.5
  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  // Exercise 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rngA) = ra(rng)
      val (b, rngB) = rb(rng)
      (f(a, b), rngB)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(nonNegativeInt, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, nonNegativeInt)

  // Exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(nonNegativeInt))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    rng => {
      val (i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      if (i + (n - 1) - mod >= 0) (mod, rng2)
      else nonNegativeLessThan(n)(rng)
    }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
    // flatMap[A, C](ra: Rand[A])(map(rb)(b => f(a, b)):(A => Rnad[C]))

  def main(args: Array[String]): Unit = {
    val r = new SimpleRNG(1)
    println(nonNegativeInt(r)._1)
    println(intDouble(r)._1)
    println(doubleInt(r)._1)
    println(double3(r)._1)
    println(ints(5)(r)._1)

    println(doubleViaMap(r)._1)

    println(randIntDouble(r)._1)
    println(randDoubleInt(r)._1)

    println(sequence[Int](List(nonNegativeInt, nonNegativeInt, nonNegativeInt))(r)._1)

    println(intsViaSequence(3)(r)._1)

    println(nonNegativeLessThan(10)(r)._1)
  }
}

import State._

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, sas2: List[State[S, A]], acc: List[A]): (List[A], S) =
      sas2 match {
        case Nil => (acc.reverse, s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }

    State((s: S) => go(s, sas, List()))
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)

  def main(args: Array[String]): Unit = {
    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    val initialMachine = Machine(true, 5, 10)
    val result = simulateMachine(inputs).run(initialMachine)._2
    println(result) // should be Machine(true, 1, 14)
  }
}
