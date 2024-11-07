/**
 * Many programming languages use exceptions as a way to short-circuit the
 * normal return process and signal failure to higher levels of an application.
 * Functional Scala provides another option: using typed return values, which
 * allow the compiler help you handle expected error cases, resulting in much
 * more robust and resilient code that better deals with the complexity of
 * life in the cloud.
 *
 * In this module, you will learn how to eliminate exceptions from your
 * application and program using typed return values.
 */
package net.degoes

import zio.test._

import scala.util.{Failure, Success, Try}

object Exceptions extends ZIOSpecDefault {
  def spec =
    suite("Exceptions") {
      suite("constructors") {

        /**
         * EXERCISE
         *
         * Modify `parseInt` to return an `Option`.
         */
        test("Option") {
          def parseInt(s: String): Option[Int] =
            try {
              Some(s.toInt)
            } catch {
              case e: NumberFormatException => None
            }

          def test = (parseInt(""): Option[Int]) match {
            case None => "None"
            case _    => "Some"
          }

          assertTrue(test == "None")
        } +
          /**
           * EXERCISE
           *
           * Modify `parseInt` to return a `Try`.
           */
          test("Try") {
            import scala.util._

            def parseInt(s: String): Try[Int] =
              try {
                Success(s.toInt)
              } catch {
                case ex: NumberFormatException => Failure(ex)
              }

            def test = (parseInt(""): Try[Int]) match {
              case Failure(_) => "Failure"
              case _          => "Success"
            }

            assertTrue(test == "Failure")
          } +
          /**
           * EXERCISE
           *
           * Modify `parseInt` to return an `Either`, where `Left` indicates
           * failure to parse an integer.
           */
          test("Either") {
            def parseInt(s: String): Either[Exception, Int] =
              try {
                Right(s.toInt)
              } catch {
                case ex: NumberFormatException => Left(ex)
              }

            def test = (parseInt(""): Any) match {
              case Left(_) => "Left"
              case _       => "Right"
            }

            assertTrue(test == "Left")
          }
      } +
        suite("map") {

          /**
           * EXERCISE
           *
           * Using `Option##map`, use the `parseInt` helper function to implement
           * a correct `Id` constructor.
           */
          test("Option") {
            def parseInt(i: String): Option[Int] =
              try Some(i.toInt)
              catch { case _: Throwable => None }

            final case class Id private (value: Int)

            object Id {
              def fromString(value: String): Option[Id] = parseInt(value) match {
                case None => None
                case res: Option[Int] => Some(new Id(res.get))
              }
            }

            assertTrue(Id.fromString("123").isDefined)
          } +
            /**
             * EXERCISE
             *
             * Using `Try#map`, use the `parseInt` helper function to implement
             * a correct `Natural.fromString` constructor, which will succeed
             * only if the string is a number, and if that number is non-negative.
             */
            test("Try") {
              import scala.util._

              def parseInt(i: String): Try[Int] = Try(i.toInt)

              final case class Id private (value: Int)

              object Id {
                def fromString(value: String): Try[Id] = {
                  parseInt(value) match {
                    case a: Success[Int] if a.value >= 0 => Success(new Id(a.value))
                    case a: Failure[Int] => Failure(a.exception)
                    case _ => Failure(new Throwable( ))
                  }
                }
              }

              assertTrue(Id.fromString("123").isSuccess)
            } +
            /**
             * EXERCISE
             *
             * Using `Either#map`, use the `parseInt` helper function to implement
             * a correct `Natural.fromString` constructor, which will succeed
             * only if the string is a number, and if that number is non-negative.
             */
            test("Either") {
              def parseInt(i: String): Either[String, Int] =
                try Right(i.toInt)
                catch {
                  case e: NumberFormatException => Left(e.getMessage())
                }

              final case class Id private (value: Int)

              object Id {
                def fromString(value: String): Either[String, Id] = {
                  parseInt(value) match {
                    case Right(result) if result >= 0 => Right(new Id(result))
                    case Left(error) => Left(error)
                    case _ => Left("Failure")
                  }
                }
              }

              assertTrue(Id.fromString("123").isRight)
            }
        } +
        suite("fallback") {

          /**
           * EXERCISE
           *
           * Implement `fallback` in such a way that it prefers the left hand
           * side, if it contains a value, otherwise, it will use the right
           * hand side.
           */
          test("Option") {
            def fallback[A](left: Option[A], right: Option[A]): Option[A] = {
              if (left.isDefined) Some(left.get)
              else if (right.isDefined) Some(right.get)
              else    None
            }

            assertTrue(fallback(None, Some(42)) == Some(42))
          } +
            /**
             * EXERCISE
             *
             * Implement `fallback` in such a way that it prefers the left hand
             * side, if it contains a value, otherwise, it will use the right
             * hand side.
             */
            test("Try") {
              import scala.util._

              def fallback[A](left: Try[A], right: Try[A]): Try[A] = {
                if (left.isSuccess) left
                else if (right.isSuccess) right
                else  new Failure[A](new Throwable("Epic Fail"))
              }

              assertTrue(fallback(Failure(new Throwable), Success(42)) == Success(42))
            } +
            /**
             * EXERCISE
             *
             * Implement `fallback` in such a way that it prefers the left hand
             * side, if it contains a value, otherwise, it will use the right
             * hand side.
             */
            test("Either") {
              def fallback[E, A](left: Either[E, A], right: Either[E, A]): Either[E, A] = {
                if (left.isRight) left
                else if (right.isRight) right
                else left
              }

              assertTrue(fallback(Left("Uh oh!"), Right(42)) == Right(42))
            }
        } +
        suite("flatMap") {

          /**
           * EXERCISE
           *
           * Using `Option##flatMap`, use the `parseInt` helper function to implement
           * a correct `Natural.fromString` constructor, which will succeed
           * only if the string is a number, and if that number is non-negative.
           */
          test("Option") {
            def parseInt(i: String): Option[Int] =
              try Some(i.toInt)
              catch { case _: Throwable => None }

            final case class Natural(value: Int)

            object Natural {
              def fromString(value: String): Option[Natural] = {
                parseInt(value).flatMap(num =>
                if (num >= 0) Some(new Natural(num))
                else  None)
              }
            }

            assertTrue(Natural.fromString("123").isDefined)
          } +
            /**
             * EXERCISE
             *
             * Using `Try#flatMap`, use the `parseInt` helper function to implement
             * a correct `Natural.fromString` constructor, which will succeed
             * only if the string is a number, and if that number is non-negative.
             */
            test("Try") {
              import scala.util._

              def parseInt(i: String): Try[Int] = Try(i.toInt)

              final case class Natural(value: Int)

              object Natural {
                def fromString(value: String): Try[Natural] = {
                  parseInt(value).flatMap(num =>
                  if (num >= 0) Success(new Natural(num))
                  else new Failure[Natural](new Throwable( )))
                }
              }

              assertTrue(Natural.fromString("123").isSuccess)
            } +
            /**
             * EXERCISE
             *
             * Using `Either##flatMap`, use the `parseInt` helper function to implement
             * a correct `Natural.fromString` constructor, which will succeed
             * only if the string is a number, and if that number is non-negative.
             */
            test("Either") {
              def parseInt(i: String): Either[String, Int] =
                try Right(i.toInt)
                catch {
                  case e: NumberFormatException => Left(e.getMessage())
                }

              final case class Natural(value: Int)

              object Natural {
                def fromString(value: String): Either[String, Natural] = {
                  parseInt(value).flatMap(num =>
                  if (num >= 0) Right(new Natural(num))
                  else          Left("Failure"))
                }
              }

              assertTrue(Natural.fromString("123").isRight)
            }
        } +
        suite("both") {

          /**
           * EXERCISE
           *
           * Implement `both` in a way that, when values are present on both
           * sides, will produce a tuple of those values.
           */
          test("Option") {
            def both[A, B](left: Option[A], right: Option[B]): Option[(A, B)] =
              if (left.isDefined && right.isDefined)  Some((left.get, right.get))
              else  None

            assertTrue(both(Some(4), Some(2)) == Some((4, 2)))
          } +
            /**
             * EXERCISE
             *
             * Implement `both` in a way that, when values are present on both
             * sides, will produce a tuple of those values.
             */
            test("Try") {
              import scala.util._

              def both[A, B](left: Try[A], right: Try[B]): Try[(A, B)] =
                if (left.isSuccess && right.isSuccess)  Success((left.get, right.get))
                else  Failure(new Throwable( ))

              assertTrue(both(Try(4), Try(2)) == Try((4, 2)))
            } +
            /**
             * EXERCISE
             *
             * Implement `both` in a way that, when values are present on both
             * sides, will produce a tuple of those values.
             */
            test("Either") {
              def both[E, A, B](left: Either[E, A], right: Either[E, B]): Either[E, (A, B)] = {
                if (left.isLeft)  Left(left.swap.toOption.get)
                else if (right.isLeft) Left(right.swap.toOption.get)
                else Right((left.merge.asInstanceOf[A], right.merge.asInstanceOf[B]))
              }

              assertTrue(both(Right(4), Right(2)) == Right((4, 2)))
            }
        } +
        suite("porting") {

          /**
           * EXERCISE
           *
           * Rewrite the following code to use `Option` instead of exceptions.
           */
          test("Option") {
            object Config {
              def getHost(): Option[String] = {
                val result = System.getProperty("CONFIG_HOST")

                if (result == null) return None

                Some(result)
              }

              def getPort(): Option[Int] = {
                val result = System.getProperty("CONFIG_PORT")

                if (result == null) return None

                Some(result.toInt)
              }
            }

            final case class ConnectionInfo(host: String, port: Int)

            def loadConnectionInfo(): ConnectionInfo =
              ConnectionInfo(Config.getHost().get, Config.getPort().get)

            assertTrue(loadConnectionInfo().FIXME)
          } +
            /**
             * EXERCISE
             *
             * Rewrite the following code to use `Try` instead of exceptions.
             */
            test("Try") {
              object Config {
                def getHost(): Try[String] = System.getProperty("CONFIG_HOST") match {
                  case null => Failure(new Throwable())
                  case res => Success(res)
                }

                def getPort(): Try[Int] = System.getProperty("CONFIG_PORT") match {
                  case null => Failure(new Throwable( ))
                  case res => Success(res.toInt)
                }
              }

              final case class ConnectionInfo(host: String, port: Int)

              def loadConnectionInfo(): ConnectionInfo =
                ConnectionInfo(Config.getHost().get, Config.getPort().get)

              assertTrue(loadConnectionInfo().FIXME)
            } +
            /**
             * EXERCISE
             *
             * Rewrite the following code to use `Either` instead of exceptions.
             */
            test("Either") {
              object Config {
                def getHost(): Either[String, String] = {
                  val result = System.getProperty("CONFIG_HOST")

                  if (result == null) return Left("Epic Failure")

                  Right(result)
                }

                def getPort(): Either[String, Int] = {
                  val result = System.getProperty("CONFIG_PORT")

                  if (result == null) return Left("Epic Failure")

                  Right(result.toInt)
                }
              }

              final case class ConnectionInfo(host: String, port: Int)

              def loadConnectionInfo(): ConnectionInfo =
                ConnectionInfo(Config.getHost().getOrElse("Fail"), Config.getPort().getOrElse(0))

              assertTrue(loadConnectionInfo().FIXME)
            }
        } +
        suite("mixed") {

          /**
           * EXERCISE
           *
           * Find a way to combine an Option and a Try in a way that loses no
           * information.
           */
          test("Option/Try") {
            import scala.util._

            type User = String
            type Docs = List[String]

            def getUser: Option[User] = Some("sherlock@holmes.com")
            def getDocs: Try[Docs]    = Try(List("Doc 1", "Doc 2"))

            def getUserAndDocs: (String, List[String]) = (getUser.get, getDocs.get)

            assertTrue(getUserAndDocs == ("sherlock@holmes.com", List("Doc 1", "Doc 2")))
          } +
            /**
             * EXERCISE
             *
             * Find a way to combine an Either and an Option in a way that loses
             * no information.
             */
            test("Either/Option") {
              import scala.util._

              type User = String
              type Docs = List[String]

              def getUser: Either[String, User] = Right("sherlock@holmes.com")
              def getDocs: Option[Docs]         = Some(List("Doc 1", "Doc 2"))

              def getUserAndDocs: (String, List[String]) = (getUser.getOrElse("Failure"), getDocs.get)

              assertTrue(getUserAndDocs == ("sherlock@holmes.com", List("Doc 1", "Doc 2")))
            } +
            /**
             * EXERCISE
             *
             * Find a way to combine an Either and a Try in a way that loses
             * no information.
             */
            test("Either/Try") {
              import scala.util._

              type User = String
              type Docs = List[String]

              def getUser: Either[String, User] = Right("sherlock@holmes.com")
              def getDocs: Try[Docs]            = Try(List("Doc 1", "Doc 2"))

              def getUserAndDocs: (String, List[String]) = (getUser.getOrElse("Failure"), getDocs.get)

              assertTrue(getUserAndDocs == ("sherlock@holmes.com", List("Doc 1", "Doc 2")))
            } +
            /**
             * EXERCISE
             *
             * Find a way to combine an Either, a Try, and an Option in a way
             * that loses no information.
             */
            test("Either/Try/Option") {
              import scala.util._

              type User  = String
              type Docs  = List[String]
              type Prefs = Map[String, Boolean]

              def getUser: Either[String, User] = Right("sherlock@holmes.com")
              def getDocs: Try[Docs]            = Try(List("Doc 1", "Doc 2"))
              def getPrefs: Option[Prefs]       = Some(Map("autosave" -> true))

              def getUserAndDocsAndPrefs = (
                getUser match {
                  case Right(value) => value
                  case _ => "Failure"
                },
                getDocs match {
                  case docs:Success[Docs] => docs.value
                  case _ => List.empty
                },
                getPrefs match {
                  case None => Map
                  case Some(value) => value
                }
              )

              assertTrue(getUserAndDocsAndPrefs == ("sherlock@holmes.com", List("Doc 1", "Doc 2"), Map("autosave" -> true)))
            }
        }
    }
}
