package fpinscala.laziness

import Stream._

trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // Exercise 5.1
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List()).reverse
  }

  // Exercise 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // Exercise 5.5
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  // Exercise 5.6
  def headOption2: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // Exercise 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B>:A](x: => Stream[B]): Stream[B] =
    foldRight(x)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) =>f(a).append(b))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption2

  // Exercise 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(a, b) => Some((f(a()), b()))
      case _ => None
    }

   def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(a, b), 1) => Some((a(), (empty, 0)))
      case (Cons(a, b), n) if n > 1 => Some((a(), (b(), n-1)))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(a, b) if f(a()) => Some((a(), b()))
      case _ => None
    }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  // Exercise 5.14
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h1 ,h2) => h1 == h2
    }

  // Exercise 5.15
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  // Exercise 5.16
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    // p0: (A, Stream[A])
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](H: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  // Smart constructor
  def cons[A](hd: =>A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  // Exercise 5.8
  def constant[A](a: A): Stream[A] = {
    // cons(a, constant(a))
    // The following is more efficient because it can refer itself
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  // Exercise 5.9
  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  // Exercise 5.10
  def fibs(): Stream[Int] = {
    def go(m: Int, n: Int): Stream[Int] =
      cons(m+n, go(n, m+n))

    go(0,1)
  }

  // Exercise 5.11
  // corecursive
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }

   // Exercise 5.12
  def fibs2: Stream[Int] =
    unfold((0, 1))(s => s match {
      case (f0, f1) => Some((f0, (f1, f0 + f1)))
    })

  def from2(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n + 1)))

  def constant2(n: Int): Stream[Int] =
    unfold(n)(_ => Some((n, n)))

  def main(args: Array[String]): Unit = {
    println(Stream(1,2,3).headOption)
    println(Stream(1,2,3).toList)
    println(Stream(1,2,3,4,5,6).take(3).toList)
    println(Stream(1,2,3,4,5,6).drop(3).toList)
    println(Stream(1,2,3,1,2,3,4,5,6).takeWhile(x => x < 4).toList)

    println(Stream(1,2,3,1,2,3,4,5,6).exists2(x => x > 6))
    println(Stream(1,2,3,1,2,3,4,5,6).exists2(x => x == 3))

    println(Stream(1,2,3,1,2,3,4,5,6).forAll(x => x <= 6))
    println(Stream(1,2,3,1,2,3,4,5,6).forAll(x => x == 3))

    println(Stream(1).headOption2)
    println(Stream().headOption2)

    println(Stream(1,2,3,1,2,3,4,5,6).filter(x => x % 3 == 0).toList)
    println(Stream(1,2,3,1,2,3).flatMap(x => Stream(x, 0)).toList)

    println(Stream(1,2,3,1,2,3).find(_ == 3))
    println(Stream(1,2,3,1,2,3).find(_ == 4))

    println(fibs().take(20).toList)

    println(Stream(1,2,3,4,5,6).takeViaUnfold(3).toList)

    println(Stream(1,2,3,4,5).zipWith(Stream(-1, -2, -3))((a, b) => (a, b)).toList)
    println(Stream(1,2,3,4,5).zipWithAll(Stream(-1, -2, -3))((a, b) => (a, b)).toList)
  }
}
