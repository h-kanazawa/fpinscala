package fpinscala.localeffects

sealed trait ST[S, A] { self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S, A](a: => A) = {
    lazy val memo = a
    new ST[S, A] {
      def run(s: S) = (memo, s)
    }
  }

  def runST[A](st: RunnableST[A]): A =
    st.apply[Unit].run(())._1
}

sealed trait STRef[S, A] {
  protected var cell: A

  def read: ST[S, A] = ST(cell)

  def write(a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
    var cell = a
  })
}

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}

sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]

  def size: ST[S, Int] = ST(value.size)

  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S) = {
      value(i) = a
      ((), s)
    }
  }

  def read(i: Int): ST[S, A] = ST(value(i))

  def freeze: ST[S, List[A]] = ST(value.toList)

  def fill(xs: Map[Int, A]): ST[S, Unit] =
    xs.foldRight(ST[S, Unit](())) {
      case ((k, v), st) => st flatMap (_ => write(k, v))
    }

  def swap(i: Int, j: Int): ST[S, Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()
}

object STArray {
  def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value = Array.fill(sz)(v)
    })

  def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value = xs.toArray
    })
}

object Immutable {
  def noop[S] = ST[S,Unit](())

  def partition[S](a: STArray[S, Int], l: Int, r: Int, pivot: Int): ST[S, Int] = for {
    vp <- a.read(pivot)
    _ <- a.swap(pivot, r)
    j <- STRef(l)
    _ <- (l until r).foldLeft(noop[S])((s, i) => for {
      _ <- s
      vi <- a.read(i)
      _  <- if (vi < vp) for {
        vj <- j.read
        _  <- a.swap(i, vj)
        _  <- j.write(vj + 1)
      } yield () else noop[S]
    } yield ())
    x <- j.read
    _ <- a.swap(x, r)
  } yield x

  def qs[S](a: STArray[S, Int], l: Int, r: Int): ST[S, Unit] = if (l < r) for {
    pi <- partition(a, l, r, l + (r - l) / 2)
    _ <- qs(a, l, pi - 1)
    _ <- qs(a, pi + 1, r)
  } yield () else noop[S]

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
      def apply[S] = for {
        arr    <- STArray.fromList(xs)
        size   <- arr.size
        _      <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      } yield sorted
    })
}

import scala.collection.mutable.HashMap

sealed trait STMap[S, K, V] {
  protected def table: HashMap[K, V]

  def size: ST[S, Int] = ST(table.size)

  def apply(k: K): ST[S, V] = ST(table(k))

  def get(k: K): ST[S, Option[V]] = ST(table.get(k))

  def +=(kv: (K, V)): ST[S, Unit] = ST(table += kv)

  def -=(k: K): ST[S, Unit] = ST(table -= k)
}

object STMap {
  def empty[S, K, V]: ST[S, STMap[S, K, V]] = ST(new STMap[S, K, V] {
    val table = HashMap.empty[K, V]
  })

  def fromMap[S, K, V](m: Map[K, V]): ST[S, STMap[S, K, V]] = ST(new STMap[S, K, V] {
    val table = (HashMap.newBuilder[K, V] ++= m).result
  })
}


object LocalEffects {
  def main(args: Array[String]): Unit = {
    val p1 = new RunnableST[(Int, Int)] {
      def apply[S] = for {
        r1 <- STRef(1)
        r2 <- STRef(2)
        x <- r1.read
        y <- r2.read
        _ <- r1.write(y + 1)
        _ <- r2.write(x + 1)
        a <- r1.read
        b <- r2.read
      } yield (a, b)
    }
    val r1 = ST.runST(p1)
    println(r1)

    val p2 = new RunnableST[List[String]] {
      def apply[S] = for {
        a <- STArray(3, "initial")
        _ <- a.fill(Map(2 -> "foo", 0 -> "bar"))
        l <- a.freeze
      } yield l
    }
    val r2 = ST.runST(p2)
    println(r2)

    val p3 = new RunnableST[List[String]] {
      def apply[S] = for {
        a <- STArray.fromList(List("aa", "bb", "cc"))
        l <- a.freeze
      } yield l
    }
    val r3 = ST.runST(p3)
    println(r3)

    val r4 = Immutable.quicksort(List(15, 3, 0, -5, 2, 1, 8, 42, 42, -9, -14, 0, 3))
    println(r4)

    val p5 = new RunnableST[(Option[Int], Option[Int], Int)] {
      def apply[S] = for {
        m <- STMap.fromMap(Map("a" -> 2, "bb" -> 3, "ccc" -> 7))
        _ <- m += ("d", 10)
        _ <- m -= ("bb")
        a <- m.get("a")
        bb <- m.get("bb")
        s <- m.size
      } yield (a, bb, s)
    }

    val r5 = ST.runST(p5)
    println(r5._1)
    println(r5._2)
    println(r5._3)
  }
}