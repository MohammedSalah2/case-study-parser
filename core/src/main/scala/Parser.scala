package parser

import parser.Parser
import cats.implicits._
import cats.{Functor, Monoid, Semigroup}

import java.beans.Expression



sealed trait Parser[A] {
  import Parser._

  def map[B](f: A => B): Parser[B] =
    ParserMap(this, f)

  def flatMap[B](f: A => Parser[B]): Parser[B] =
    ParserFlatMap(this, f)

  def orElse(that: => Parser[A]): Parser[A] =
    ParserOrElse(this, that)

  def product[B](that: Parser[B]): Parser[(A, B)] =
    ParserProduct(this, that)

//  def and(that: Parser[A])(implicit m: Semigroup[A]): Parser[A]
//
//  def repeat(implicit m: Monoid[A]): Parser[A]



  def parse(input: String): Result[A] = {
    def loop[A](parser: Parser[A], index: Int): Result[A] =
      parser match {

        case ParserMap(source, f) =>
          loop(source, index) match {
            case Failure(reason, input, start) => Failure(reason, input, start)
            case Success(result, input, offset) =>
              Success(f(result: Any), input, offset)
          }

        case ParserString(value) =>
          if (input.startsWith(value, index))
            Success(value, input, index + value.size)
          else
            Failure(
              s"input did not start with $value at index $index",
              input,
              index
            )

        case ParserFlatMap(source, f) =>
          loop(source, index) match {
            case Failure(reason, input, start) => Failure(reason, input, start)
            case Success(result, input, offset) =>
              loop(f(result), offset)
          }


        case ParserOrElse(source, that) =>
          loop(source, index) match {
            case Failure(reason, input, start) => loop(that, index)
            case Success(result, input, offset) =>
              Success(result, input, offset)
          }

        case ParserProduct(source, that) =>
          loop(source, index) match {
            case Failure(reason, input, start) => Failure(reason, input, start)
            case Success(resultA, _, offset) =>
              loop(that, offset) match {
                case Failure(reason, input, start) =>
                  Failure(reason, input, start)
                case Success(resultB, input, offset) =>
                  Success((resultA, resultB), input, offset)
              }
          }

      }

    loop(this, 0)
  }
}
object Parser {
  def string(value: String): Parser[String] = ParserString(value)
  def pure[A](value: A): Parser[A]
  def fail[A]: Parser[A]
//  def pass[A](implicit m: Monoid): Parser[A]
//  def and[A](that: Parser[A])(implicit m: Semigroup[A]): Parser[A] = ParserAnd(that)

  final case class ParserMap[A, B](source: Parser[A], f: A => B) extends Parser[B]
  final case class ParserString(value: String) extends Parser[String]
  final case class ParserFlatMap[A, B](source: Parser[A], f: A => Parser[B]) extends Parser[B]
  final case class ParserOrElse[A](source: Parser[A], that: Parser[A]) extends Parser[A]
  final case class ParserProduct[A, B](source: Parser[A], that: Parser[B]) extends Parser[(A, B)]
  final case class ParserAnd[A](that: Parser[A])(implicit val m: Semigroup[A]) extends Parser[A]
  final case class ParserRepeat[A](source: Parser[A])(implicit val m: Monoid[A]) extends Parser[A]


  implicit val parserFunctorInstance: Functor[Parser] =
    new Functor[Parser] {
      def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
        fa.map(f)
    }


  val alphabetic: Parser[String] =
    List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
      'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')
      .foldLeft(Parser.fail[String]) { (parser, char) => parser.orElse(Parser.string(char.toString))}

//  val variable = alphabetic.and(alphabetic.repeat)
//
//  val digit: Parser[String] =
//    List(1, 2, 3, 4, 5, 6, 7, 8, 9)
//      .foldLeft(Parser.string("0")) { (accum, digit) =>
//        accum.orElse(Parser.string(digit.toString))
//
//        val literal: Parser[Expression] =
//          digit.and(digit.repeat).map(myInt => Expression.literal(myInt.toInt))
//
//      }