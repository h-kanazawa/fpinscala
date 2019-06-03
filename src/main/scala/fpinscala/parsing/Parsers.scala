package fpinscala.parsing

import org.scalatest.FunSuite

import java.util.regex.Pattern
import scala.util.matching.Regex


trait Parsers[Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  def succeed[A](a: A): Parser[A]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(a => succeed(f(a)))

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(p)(a => map(p2)(b => f(a, b)))

  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(p, p2)((_, b) => b)

  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, p2)((a, _) => a)

  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) =
    skipR(skipL(start, p), stop)

  def many[A](p: Parser[A]): Parser[List[A]] =
    or(map2(p, many(p))(_ :: _), succeed(List()))

  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(skipL(p2, p)))(_ :: _)

  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    or(sep1(p, p2), succeed(List()))

  def int: Parser[Int] =
    map(token("[1-9][0-9]*".r))(_.toInt)

  def double: Parser[Double] =
    map(token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r))(_.toDouble)

  def whitespace: Parser[String] = "\\s*".r

  def token[A](p: Parser[A]): Parser[A] =
    skipR(p, whitespace)

  def thru(s: String): Parser[String] =
    (".*?" + Pattern.quote(s)).r

  def quoted: Parser[String] =
    skipL(string("\""), map(thru("\""))(_.dropRight(1)))

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p)(a => map(p2)(b => (a, b)))
  // for { a <- p; b <- p2 } yield (a, b)
}

case class ParseError(msg: String)

trait JSON

object JSON {

  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    //import P._
    // hide string implicit conversion and use token
    import P.{string => _, _}
    implicit def tok(s: String) = token(P.string(s))

    def bool = or(
      map("true")(_ => JBool(true)),
      map("false")(_ => JBool(false))
    )

    def str: Parser[JSON.JString] =
      map(quoted)(JString(_))

    def lit =
      or (
        map(double)(JNumber(_)),
        or (
          map("null")(_ => JNull),
          or (
            bool,
            str
          )
        )
      )

    def value: Parser[JSON] = or(or(lit, array), obj)

    def array =
      surround("[","]")(
        map(sep(value, ","))(vs => JArray(vs.toIndexedSeq))
      )

    def keyval = product(quoted, (skipL(":", value)))

    def obj =
      surround("{","}")(
        map(sep(keyval, ","))(kvs => JObject(kvs.toMap))
      )

    skipL(whitespace, or(array, obj))
  }
}

object ReferenceTypes {
  case class ParseState(source: String, offset: Int) {
    def advanceBy(numChars: Int): ParseState =
      copy(offset = offset + numChars)

    def input: String = source.substring(offset)

    def slice(n: Int): String = source.substring(offset, offset + n)
  }

  trait Result[+A] {
    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, n + m)
      case _ => this
    }
  }

  case class Success[+A](get: A, length: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]

  type Parser[+A] = ParseState => Result[A]
}

import fpinscala.parsing.ReferenceTypes._

object Reference extends Parsers[Parser] {
  def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(ParseState(input, 0)) match {
      case Failure(parseError) => Left(parseError)
      case Success(a, _) => Right(a)
    }

  implicit def string(w: String): Parser[String] =
    s => if (s.input.startsWith(w)) Success(w, w.length)
    else Failure(ParseError("Not match string"))

  implicit def regex(r: Regex): Parser[String] =
    s => r.findPrefixOf(s.input) match {
      case Some(m) => Success(m, m.length)
      case None =>  Failure(ParseError("Not match regex"))
    }

  def succeed[A](a: A): Parser[A] =
    _ => Success(a, 0)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
    s => p(s) match {
      case Success(ma, la) =>
        f(ma)(s.advanceBy(la)).advanceSuccess(la)
      case f@Failure(_) => f
    }

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] =
    s => p1(s) match {
      case Failure(_) => p2(s)
      case r => r
    }

  def slice[A](p: Parser[A]): Parser[String] =
    s => p(s) match {
      case Success(_, n) => Success(s.slice(n), n)
      case f@Failure(_) => f
    }
}

class Test extends FunSuite {
  val R = Reference
  val jsonParser: Parser[JSON] = JSON.jsonParser(R)

  test("Test 3") {
    import Reference._
    val actual = R.run(or(string("a"), int))("a12")
    val expected = Right("a")
    assert(actual == expected)
  }

  test("Test 4") {
    import Reference._
    val actual = R.run(or(string("a"), int))("12a")
    val expected = Right(12)
    assert(actual == expected)
  }

  test("Test 5") {
    import Reference._
    val actual = string("b")(ParseState("abc", 1))
    val expected = Success("b", 1)
    assert(actual == expected)
  }

  test("Test 6") {
    import Reference._
    val actual = string("bc")(ParseState("abcd", 1))
    val expected = Success("bc", 2)
    assert(actual == expected)
  }

  test("Test 7") {
    import Reference._
    val actual = slice(string("de"))(ParseState("abcdefghi", 3))
    val expected = Success("de", 2)
    assert(actual == expected)
  }

  test("Test 8") {
    import Reference._
    val actual = skipL(string("ab"), string("cde"))(ParseState("xabcde", 1))
    val expected = Success("cde", 5)
    assert(actual == expected)
  }

  test("Test 9") {
    import Reference._
    val actual = skipR(string("ab"), string("cde"))(ParseState("xabcde", 1))
    val expected = Success("ab", 5)
    assert(actual == expected)
  }

  test("Test 10") {
    import Reference._
    val actual = or(string("ab"), string("cde"))(ParseState("xabcde", 1))
    val expected = Success("ab", 2)
    assert(actual == expected)
  }

  test("Test 11") {
    import Reference._
    val actual = or(string("cde"), string("ab"))(ParseState("xabcde", 1))
    val expected = Success("ab", 2)
    assert(actual == expected)
  }

  test("Test 12") {
    import Reference._
    val actual = surround(string("ab"), string("cde"))(string("fghi"))(ParseState("xabfghicde", 1))
    val expected = Success("fghi", 9)
    assert(actual == expected)
  }

  test("Test 13") {
    import Reference._
    val actual = many(string("ab"))(ParseState("ababab", 0))
    val expected = Success(List("ab", "ab", "ab"), 6)
    assert(actual == expected)
  }

  test("Test 14") {
    import Reference._
    val actual = sep1(string("a"), string(",") )(ParseState("a,a,a", 0))
    val expected = Success(List("a", "a", "a"), 5)
    assert(actual == expected)
  }

  test("Test 15") {
    import Reference._
    val actual = sep(string("a"), string(",") )(ParseState("", 0))
    val expected = Success(List(), 0)
    assert(actual == expected)
  }

  test("Test 16") {
    import Reference._
    val actual = int(ParseState("123", 0))
    val expected = Success(123, 3)
    assert(actual == expected)
  }

  test("Test 17") {
    import Reference._
    assert(double(ParseState("123.45", 0)) ==  Success(123.45, 6))
    assert(double(ParseState("-0.01", 0)) ==  Success(-0.01, 5))
    assert(double(ParseState("9e-3", 0)) ==  Success(0.009, 4))
  }

  test("JSON 1") {
    val actual = R.run(jsonParser)("[]")
    val expected = Right(JSON.JArray(IndexedSeq()))
    assert(actual == expected)
  }

  test("JSON 2") {
    val actual = R.run(jsonParser)(
      """
        |[
        |  1,
        |  true,
        |  [ null, 3,  false ],
        |  4
        |] """.stripMargin)
    val expected = Right(
      JSON.JArray(IndexedSeq(
        JSON.JNumber(1),
        JSON.JBool(true),
        JSON.JArray(IndexedSeq(
          JSON.JNull,
          JSON.JNumber(3),
          JSON.JBool(false)
        )),
        JSON.JNumber(4),
      )
      )
    )
    assert(actual == expected)
  }

  test("JSON 3") {
    val input = "[\"abc\",\"d e\"]"
    val actual = R.run(jsonParser)(input)
    val expected = Right(
      JSON.JArray(IndexedSeq(
        JSON.JString("abc"),
        JSON.JString("d e")
      ))
    )
    assert(actual == expected)
  }

  test("JSON 4") {
    val input = "{\"a\":\"b\",\"c\":{\"d\":1,\"e\":null}}"
    val actual = R.run(jsonParser)(input)
    val expected = Right(
      JSON.JObject(Map(
        ("a", JSON.JString("b")),
        ("c", JSON.JObject(Map(
          ("d", JSON.JNumber(1)),
          ("e", JSON.JNull)
        )))
      ))
    )
    assert(actual == expected)
  }
}
