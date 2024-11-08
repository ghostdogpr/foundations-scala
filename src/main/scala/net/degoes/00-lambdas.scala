/**
 * Functions are fundamental to functional programming. In a functional
 * programming language, functions are values, which may be stored in
 * fields, accepted as arguments to other functions, and returned from
 * functions. So-called "first-class functions" go by many names in
 * different programming languages, including "closures", "lambdas",
 * and "anonymous functions". Effectively understanding and using
 * functions is a pre-requisite to the mastery of functional
 * programming, in Scala or any other programming language.
 *
 * In this module, you will learn about "functions as values" and become
 * comfortable creating, transforming, composing, and typing functions.
 */
package net.degoes.lambdas

import zio._
import zio.test._
import zio.test.TestAspect._

/*

object Lambdas extends ZIOSpecDefault {
  def assertTypeEquals[A, B](implicit ev: A <:< B) = assertCompletes

  def spec =
    suite("Lambdas") {
      suite("values") {

        /**
         * EXERCISE
         *
         * Create a lambda which squares its argument and store the lambda into
         * the `square` variable
         */
        test("square") {
          val square: Int => Int = a => a * a

          assertTrue(square(3) == 9)
        } +
          test("plus") {

            /**
             * EXERCISE
             *
             * Create a lambda with two arguments, which adds them together, and
             * store the lambda into the `plus` variable.
             */
            val plus: (Int, Int) => Int = (a, b) => a + b

            assertTrue(plus(2, 2) == 4)
          } +
          /**
           * EXERCISE
           *
           * Use the `_` to create a lambda that adds one to its argument.
           */
          test("underscore") {
            val addTwo: Int => Int = _ + 2

            assertTrue(addTwo(2) == 4)
          } +
          /**
           * EXERCISE
           *
           * Use `Function#andThen` to compose two of the following functions
           * together to create a composite function that counts the number of
           * digits inside an integer.
           */
          test("andThen") {
            val convertToString: Int => String = _.toString
            val countLength: String => Int     = _.length

            val _ = (convertToString, countLength)

            val numberOfDigits: Int => Int = convertToString.andThen(countLength)

            assertTrue(numberOfDigits(123) == 3)
          } +
          /**
           * EXERCISE
           *
           * Use `Function#compose` to compose two of the following functions
           * together to create a composite function that counts the number of
           * digits inside an integer.
           *
           * Which one do you prefer, `compose` or `andThen`?
           */
          test("compose") {
            val convertToString: Int => String = _.toString
            val countLength: String => Int     = _.length

            val _ = (convertToString, countLength)

            val numberOfDigits: Int => Int = countLength.compose(convertToString)

            assertTrue(numberOfDigits(123) == 3)
          } +
          /**
           * EXERCISE
           *
           * Use `identity` to create a function that returns its same string
           * argument.
           */
          test("identity") {
            val sameString: String => String = a => a

            assertTrue(sameString("foobar") == "foobar" && sameString("barfoo") == "barfoo")
          } +
          /**
           * EXERCISE
           *
           * Use `Function.const` to create a function that always returns 42,
           * regardless of whichever string argument is passed.
           */
          test("const") {
            val answer: String => Int = _ => 42

            assertTrue(answer("foo") == answer("bar") && answer("foobar") == 42)
          } +
          /**
           * EXERCISE
           *
           * Create a function which returns a function. The returned function
           * should prepend the specified number of spaces to the string that it
           * is passed.
           */
          test("prependSpace") {
            val prependSpace: Int => (String => String) =
              spaceNumber => (inputStr => " " * spaceNumber + inputStr) : String => String

            assertTrue(prependSpace(5)("foo") == "     foo")
          } +
          /**
           * EXERCISE
           *
           * Create a function that returns a function transformer. The function
           * transformer should take a `String => String` function, and then
           * return a new `String => String` function that will repeat the original
           * function the specified number of times.
           */
          test("repeat") {
            val repeat: Int => (String => String) => (String => String) = ???

            assertTrue(repeat(5)(acc => acc + ".")("Coming soon") == "Coming soon.....")
          }
      } +
        suite("types") {

          /**
           * EXERCISE
           *
           * Determine the type of `f`, and place this (function) type in the
           * space provided. If you are correct, then the test will compile.
           */
          test("example 1") {
            val f = (x: Int) => x * x

            type Type = Int => Int

            assertTypeEquals[f.type, Type]
          } +
            /**
             * EXERCISE
             *
             * Determine the type of `f`, and place this (function) type in the
             * space provided. If you are correct, then the test will compile.
             */
            test("example 2") {
              val f = (x: Int, y: Int) => x + y

              type Type = (Int, Int) => Int

              assertTypeEquals[f.type, Type]
            } +
            /**
             * EXERCISE
             *
             * Determine the type of `f`, and place this (function) type in the
             * space provided. If you are correct, then the test will compile.
             */
            test("example 3") {
              val f = (t: (Int, Int)) => t._1 + t._2

              type Type = ((Int, Int)) => Int

              assertTypeEquals[f.type, Type]
            } +
            /**
             * EXERCISE
             *
             * Determine the type of `f`, and place this (function) type in the
             * space provided. If you are correct, then the test will compile.
             */
            test("example 4") {
              val f = (x: Int) => (y: Int) => x + y

              type Type = Int => Int => Int

              assertTypeEquals[f.type, Type]
            } +
            /**
             * EXERCISE
             *
             * Determine the type of `f`, and place this (function) type in the
             * space provided. If you are correct, then the test will compile.
             */
            test("example 5") {
              val f = (x: Int) => (g: Int => Int) => g(x)

              type Type = Int => (Int => Int) => Int

              assertTypeEquals[f.type, Type]
            }
        } +
        suite("partiality") {

          /**
           * EXERCISE
           *
           * Define a `divide` partial function that is only defined when the
           * second component of the tuple is non-zero.
           */
          test("divide") {
            val divide: PartialFunction[(Int, Int), Int] = {
              case (t: (Int, Int)) if t._2 != 0 => t._1 / t._2
            }

            assertTrue(!divide.isDefinedAt((42, 0)))
          } +
            /**
             * EXERCISE
             *
             * Define a `divideOption` method by using the
             * `PartialFunction#lift` method.
             */
            test("lift") {
              val divide: PartialFunction[(Int, Int), Int] = {
                case (x, y) if y != 0 => x / y
              }

              val _ = divide

              def divideOption: ((Int, Int)) => Option[Int] =
                (t: (Int, Int)) => divide.lift(t)

              assertTrue(divideOption((42, 0)) == None)
            }
        }
    }
}

/**
 * Parsers are a great example of the power of lambdas. A parser can be viewed
 * as nothing more than a lambda. Functions can construct or combine parsers,
 * providing the ability to compositionally specify how to parse any type of
 * data.
 *
 * In this graduation project, you will gain experience constructing lambdas,
 * including higher-order lambdas which themselves accept and return lambdas.
 */
object LambdasGraduation extends ZIOAppDefault {
  type Parser[+A] = String => Either[String, (String, A)]

  object Parser {

    /**
     * EXERCISE
     *
     * Implement a parser that succeeds with the specified value, but does not
     * consume any input.
     */
    def succeed[A](a: => A): Parser[A] = s => Right((s, a))

    /**
     * EXERCISE
     *
     * Implement a parser that fails with the specified message, and does not
     * consume any input.
     */
    def fail(message: => String): Parser[Nothing] = _ => Left(message)

    /**
     * EXERCISE
     *
     * Implement a parser that consumes any character, or fails if there are
     * no characters left to consume.
     */
    def anyChar: Parser[Char] = ???

    /**
     * EXERCISE
     *
     * Implement a parser that parses only the specified character, or fails
     * with a message indicating which character was expected.
     */
    def char(char: Char): Parser[Unit] = char => Right(char, Unit)
  }

  implicit class ParserExtensionMethods[A](self: Parser[A]) {

    def map[B](f: A => B): Parser[B] = self.flatMap(a => Parser.succeed(f(a)))

    /**
     * EXERCISE
     *
     * Implement a function that can feed the output value of this parser
     * into the provided callback, which can return a new parser which
     * will be fed the leftover input of this parser.
     */
    def flatMap[B](f: A => Parser[B]): Parser[B] = ???

    def ~[B](that: => Parser[B]): Parser[(A, B)] =
      self.flatMap(a => that.map(b => (a, b)))

    def <~(that: => Parser[Any]): Parser[A] = (self ~ that).map(_._1)

    def ~>[B](that: => Parser[B]): Parser[B] = (self ~ that).map(_._2)

    /**
     * EXERCISE
     *
     * Implement a function that will try to parse using the left-hand side,
     * but if that fails, it will try to parse using the right-hand side.
     */
    def |(that: => Parser[A]): Parser[A] = ???

    def repeat: Parser[List[A]] =
      (self ~ repeat).map {
        case (head, tail) => head :: tail
      } | Parser.succeed(List.empty[A])

    def optional: Parser[Option[A]] =
      self.map(Option(_)) | Parser.succeed(Option.empty[A])

    def run(input: String): Either[String, A] = self(input).map(_._2)
  }

  def readFile(file: String): Task[String] =
    ZIO.attempt {
      val source = scala.io.Source.fromFile(file)

      try {
        source.getLines().mkString("\n")
      } finally {
        source.close()
      }
    }

  sealed trait CSVData
  object CSVData {
    final case class Header(names: List[String])   extends CSVData
    final case class Values(columns: List[String]) extends CSVData
  }

  /**
   * EXERCISE
   *
   * Implement a function to parse the contents of a CSV file into  alist of
   * CSV data elements.
   */
  def parseFile(contents: String): Either[String, List[CSVData]] = ???

  def run =
    for {
      args     <- getArgs
      contents <- readFile(args(0))
      parsed   <- ZIO.from(parseFile(contents)).mapError(e => new RuntimeException(e))
      _        <- Console.printLine(parsed.mkString("\n"))
    } yield ()
}
*/