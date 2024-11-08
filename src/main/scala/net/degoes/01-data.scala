/**
 * In functional Scala, nearly all data is immutable. Data models are
 * constructed entirely from either "records", which have multiple fields
 * of different types, or "enumerations", which have multiple cases with
 * different structure. Scala provides powerful functionality built into
 * records and enumerations (Scala 3 only). Everything about the way you
 * construct these data models to the way you use them is different than
 * in an object-oriented programming language. The emphasis is on making
 * very precise data models that cannot be used to store "bad data", which
 * results in eliminating runtime errors and keeping bad data out of
 * databases and third-party systems.
 *
 * In this module, you will learn how to adopt the functional tools that
 * Scala gives you to solve data modeling problems in a precise way that
 * improves the maintainability and reliability of your software.
 */
package net.degoes

import zio._
import zio.test._
import zio.test.TestAspect._

/*

object Data extends ZIOSpecDefault {
  type ???

  def spec =
    suite("Data") {
      suite("Case Classes") {

        /**
         * EXERCISE
         *
         * Create a Person case class to get free getters (fields) for all the
         * constructor parameters of the class.
         */
        test("fields") {
          class Person(var name: String, var age: Int)

          def getName(p: Person): String = p.name
          def getAge(p: Person): Int     = p.age

          val holmes = new Person("Sherlock Holmes", 42)

          assertTrue(getName(holmes) == "Sherlock Holmes" && getAge(holmes) == 42)
        } +
          /**
           * EXERCISE
           *
           * Create a Person case class with a name (String) and an age (Int),
           * and delete the fake constructor to observe the free constructor that
           * all case classes receive in their companion objects.
           */
          test("apply") {
            object Person {
              class Person(var name: String, var age: Int)

              def apply(name: String, age: Int) = new Person(name, age)
            }

            assertTrue(Person("Sherlock Holmes", 42) == Person("Sherlock Holmes", 42))
          } +
          /**
           * EXERCISE
           *
           * Get a free implementation of equality for the `Profile` class by
           * turning it into a case class.
           */
          test("equals") {
            class Profile(val age: Int) {
              override def equals(other: Any): Boolean = other match {
                case profile: Profile =>
                  this.age == profile.age
                case _ => false
              }

              override def hashCode( ): Int = age.hashCode() + 31 * age
            }

            assertTrue(new Profile(42) == new Profile(42))
          } +
          /**
           * EXERCISE
           *
           * Get a free implementation of hash code for the `CreditCard` class
           * by turning it into a case class.
           */
          test("hashCode") {
            class CreditCard(val number: String) {
              override def hashCode(): Int = super.hashCode( )
            }

            assertTrue(new CreditCard("123").hashCode == new CreditCard("123").hashCode)
          } +
          /**
           * EXERCISE
           *
           * Get a free implementation of `toString` for the `Address` class
           * by turning it into a case class.
           */
          test("toString") {
            class Address(val street: String) {
              override def toString: String = "Address(" + this.street + ")"
            }

            assertTrue(new Address("221B Baker Street").toString == "Address(221B Baker Street)")
          } +
          /**
           * EXERCISE
           *
           * Get a free implementation of `copy` for the `Permissions` class
           * by turning it into a case class.
           */
          test("copy") {
            class Permissions(val canRead: Boolean, canWrite: Boolean, canShare: Boolean) {
              def copy(
                canRead: Boolean = this.canRead,
                canWrite: Boolean = this.canWrite,
                canShare: Boolean = this.canShare
              ): Permissions = new Permissions(
                canRead, canWrite, canShare
              )
            }

            val perms = new Permissions(true, false, false)

            assertTrue(perms.copy(canRead = false) == new Permissions(false, false, false))
          } +
          suite("patterns") {

            /**
             * EXERCISE
             *
             * Use pattern matching to extract out the `street` of `Address`.
             */
            test("simple") {
              final case class Address(street: String)

              def extractStreet(address: Address): String = address.street

              assertTrue(extractStreet(Address("221B Baker")) == "221B Baker")
            } +
              /**
               * EXERCISE
               *
               * Use pattern matching to extract out the `postalCode` of
               * `Address`, using a wildcard to ignore (match any) `street`.
               */
              test("wildcard") {
                final case class Address(street: String, postalCode: String)

                def extractPostalCode(address: Address): String = address.postalCode

                assertTrue(extractPostalCode(Address("221B Baker", "NW1 6XE")) == "NW1 6XE")
              } +
              /**
               * EXERCISE
               *
               * Using pattern matching on a constant, implement the provided
               * function so that it returns true for any street matching
               * "221B Baker", regardless of postal code.
               */
              test("constant") {
                final case class Address(street: String, postalCode: String)

                def is221B(address: Address): Boolean = address.street match {
                  case "211B Baker" => true
                  case _ => false
                }

                assertTrue(is221B(Address("221B Baker", "NW1 6XE")))
              } +
              /**
               * EXERCISE
               *
               * Using multiple ordered case clauses in a pattern match,
               * implement the provided function so it returns "Knows Holmes"
               * if provided any address on "Baker" street, and "Unknown"
               * otherwise.
               */
              test("ordered") {
                final case class Address(number: String, street: String, postalCode: String)

                def neighbor(address: Address): String = address.street match {
                  case "Baker" => "Knows Holmes"
                  case _ => "Unknown"
                }

                assertTrue(neighbor(Address("220", "Baker", "NW1 6XE")) == "Knows Holmes")
              } +
              /**
               * EXERCISE
               *
               * Using conditional patterns, implement the provided function so
               * that it returns true for any street that contains "Baker".
               */
              test("conditional") {
                final case class Address(street: String, postalCode: String)

                def isBaker(address: Address): Boolean = address.street.contains("Baker")

                assertTrue(isBaker(Address("220 Baker", "NW1 6XE")))
              } +
              /**
               * EXERCISE
               *
               * Using nested patterns, implement the provided function so
               * that it extracts out the postal code of any person.
               */
              test("nested") {
                final case class Person(name: String, address: Address)
                final case class Address(street: String, postalCode: String)

                def extractPostalCode(person: Person): String = person.address.postalCode

                val sherlock = Person("Sherlock Holmes", Address("221B Baker", "NW1 6XE"))

                assertTrue(extractPostalCode(sherlock) == "NW1 6XE")
              } +
              /**
               * EXERCISE
               *
               * Using quoted constants, return true if an address street
               * matches the `sherlockStreet` constant.
               */
              test("quoted") {
                final case class Address(number: String, street: String, postalCode: String)

                val sherlockStreet = "Baker"

                val _ = sherlockStreet

                def isSherlockStreet(address: Address): Boolean = address.street == sherlockStreet

                val address = Address("220", "Baker", "NW1 6XE")

                assertTrue(isSherlockStreet(address))
              }
          }
      } +
        suite("Sealed Traits") {

          /**
           * EXERCISE
           *
           * Seal the following trait to obtain exhaustivity checking on
           * pattern matching. Notice how the warning changes.
           */
          test("sealed") {
            trait Color
            case object Red   extends Color
            case object Green extends Color
            case object Blue  extends Color

            val _ = Green
            val _ = Blue

            val isRed: Color => Boolean = {
              case Red => true
            }

            assertTrue(!isRed(Blue))
          } +
            /**
             * EXERCISE
             *
             * Create a sealed trait `Country` that is extended by case objects
             * `UK`, `Germany`, `India`, `Netherlands`, and `USA`.
             */
            test("country") {
              trait Country
              object UK
              object USA

              def isCountry(a: Any) = a.isInstanceOf[Country]

              assertTrue(isCountry(UK) && isCountry(USA))
            } +
            /**
             * EXERCISE
             *
             * In implementing the `asCreditCard` method, use an `as` pattern
             * to match against a `CreditCard` payment method, capture it as a
             * variable, and return that variable wrapped in a `Some(_)`
             * constructor. For other payment methods, return `None`.
             */
            test("as patterns") {
              sealed trait PaymentMethod
              final case class CreditCard(number: String, expDate: java.time.YearMonth, securityCode: Short)
                  extends PaymentMethod
              final case class PayPal(email: String) extends PaymentMethod

              val _ = PayPal("")

              def asCreditCard(paymentMethod: PaymentMethod): Option[CreditCard] = ???
//                paymentMethod <:< CreditCard => Some(paymentMethod)
//                _ => None
//              }

              val cc: CreditCard = CreditCard("123123123123", java.time.YearMonth.of(1984, 12), 123)

              assertTrue(asCreditCard(cc) == Option(cc))
            }
        } +
        suite("Modeling") {

          /**
           * EXERCISE
           *
           * Create a precise data model for `RelationshipStatus`, which
           * models the relationship status of an individual: married,
           * single, divorced.
           */
          test("example 1") {

            sealed trait RelationshipStatus
            object RelationshipStatus {
              case object Married extends RelationshipStatus
              case object Divorced extends RelationshipStatus
              case object Single extends RelationshipStatus
            }

            import RelationshipStatus.{Married, Single, Divorced}
            def makeMarried: RelationshipStatus = Married

            def makeSingle: RelationshipStatus = Single

            assertTrue(makeMarried != makeSingle)
          } +
            /**
             * EXERCISE
             *
             * Create a precise data model for a `PaymentProcessorAPI`, which
             * stores a connection URL, a data format (JSON or XML), and an
             * API token, which is a string.
             */
            test("example 2") {
              sealed abstract class DataFormat
              object DataFormat {
                case object JSON extends DataFormat
                case object XML extends DataFormat
              }

              import DataFormat.{JSON, XML}

              case class PaymentProcessorAPI(url: java.net.URI, df: DataFormat, apiToken: String)

              def define(url: java.net.URI, df: DataFormat, apiToken: String): PaymentProcessorAPI =
                new PaymentProcessorAPI(url, df, apiToken)

              val url              = new java.net.URI("https://stripe.com")
              def json: DataFormat = JSON

              val api1 = define(url, json, "123123")
              val api2 = define(url, json, "123124")

              assertTrue(api1 == api1 && api1 != api2)
            } +
            /**
             * EXERCISE
             *
             * Create a precise data model for a user's crypto portfolio.
             */
            test("example 3") {
              sealed abstract class Symbol
              object Symbol {
                case object Eth extends Symbol
                case object Btc extends Symbol
              }

              import Symbol.{Eth, Btc}
              class Portfolio {
                val balance = Map[Symbol, Double]()

                override def equals(obj: Any): Boolean = {
                  case obj: Portfolio => balance.keySet
                    .filter((sym: Symbol) => obj.balance.getOrElse(sym, 0) != obj.balance.getOrElse(sym, 0))
                    .size > 0
                  case _ => false
                }
              }

              def ETH: Symbol = Eth
              def BTC: Symbol = Btc

              def add(portfolio: Portfolio, symbol: Symbol, amount: Double): Portfolio = {
                portfolio.balance += (symbol, portfolio.balance.getOrElse(symbol, 0) + amount)
              }

              def empty: Portfolio = new Portfolio

              val p1 = add(add(add(empty, ETH, 1.0), ETH, 1.0), BTC, 2.0)
              val p2 = add(add(empty, BTC, 2.0), ETH, 2.0)

              assertTrue(p1 == p2)
            } +
            /**
             * EXERCISE
             *
             * Create a precise data model for a subscription for a SaaS
             * product, which could be at the annual or monthly level, and
             * which could bundle different features into the plan.
             */
            test("example 4") {
              class Features(var space: Int, val sso: Boolean, val customLogo: Boolean)
              type Subscription = ???
              def makeFeatures(space: Int, sso: Boolean, customLogo: Boolean): Features = ???
              def makeMonthly(amount: Double, features: Features): Subscription         = ???
              def makeAnnually(amount: Double, features: Features): Subscription        = ???

              val features = makeFeatures(2048, true, true)

              assertTrue(makeMonthly(9.99, features) != makeAnnually(9.99, features))
            } @@ ignore +
            /**
             * EXERCISE
             *
             * Create a precise data model for fields, which contain names
             * and field types, where field types may be integers, strings,
             * booleans, or other common types for fields in forms.
             */
            test("advanced example")  {
              class Field[A](val fieldType: FieldType[A], val name: String)
              type FieldType[A] = ???

              def intType: FieldType[Int]    = ???
              def strType: FieldType[String] = ???

              def makeField[A](name: String, fieldType: FieldType[A]): Field[A] = ???

              val strField1 = makeField("name", strType)
              val strField2 = makeField("name", strType)
              val numField  = makeField("age", intType)

              assertTrue(strField1 == strField2 && numField != strField1)
            } @@ ignore
        }
    }
}

/**
 * While many programming languages have a construct like case classes, few
 * have the power of sealed traits, and most do not have the pattern matching
 * capabilities of Scala. With the combination of these powerful features, you
 * can construct very precise data models that eliminate runtime errors and
 * make it easier than ever to test and maintain code.
 *
 * In this graduation project, you will gain experience constructing precise
 * data models using case classes and sealed traits.
 */
object DataGraduation extends ZIOAppDefault {

  sealed trait Command
  object Command {
    case object Exit                        extends Command
    final case class Look(what: String)     extends Command
    final case class Go(where: String)      extends Command
    final case class Take(what: String)     extends Command
    final case class Drop(what: String)     extends Command
    final case class Fight(who: String)     extends Command
    final case class TalkTo(who: String)    extends Command
    final case class Unknown(input: String) extends Command

    def fromString(input: String): Command =
      input.trim.toLowerCase.split("\\w+").toList match {
        case exit :: Nil                  => Exit
        case "look" :: what :: Nil        => Look(what)
        case "go" :: where :: Nil         => Go(where)
        case "take" :: what :: Nil        => Take(what)
        case "drop" :: what :: Nil        => Drop(what)
        case "fight" :: who :: Nil        => Fight(who)
        case "talk" :: "to" :: who :: Nil => TalkTo(who)
        case _                            => Unknown(input)
      }
  }

  /**
   * EXERCISE
   *
   * Construct a data model for the state of a game world in a text-based
   * role-playing game. The data model should represent the player character,
   * the map of the game world, items and characters in the game world, and
   * anything else relevant to the game.
   */
  final case class State(playerName: String)

  final case class Step(nextState: Option[State], output: String)

  /**
   * EXERCISE
   *
   * Implement the `nextStep` function in such a fashion that new states for
   * the game world are constructed from both the old state and the current
   * command read from the user.
   */
  def nextStep(state: State, command: Command): Step = ???

  def mainLoop(ref: Ref[State]) =
    (for {
      line    <- Console.readLine
      command = Command.fromString(line)
      state   <- ref.get
      step    = nextStep(state, command)
      cont    <- step.nextState.fold(ZIO.succeed(false))(ref.set(_).as(true))
      _       <- Console.printLine(step.output)
    } yield cont).repeatWhile(cont => cont == true)

  def run =
    for {
      _     <- Console.printLine("Welcome to the game! What is your name?")
      name  <- Console.readLine
      state = State(name)
      ref   <- Ref.make(state)
      _     <- mainLoop(ref)
    } yield ()
}



 */