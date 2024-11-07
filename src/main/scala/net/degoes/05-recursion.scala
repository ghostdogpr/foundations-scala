/**
 * One of the backbones of applications is the ability to loop: looping is
 * used both for most algorithms (such as sorting lists), as well as for
 * large-scale application behavior like repeatedly reading results from a
 * database query or repeatedly handling requests from a socket. Functional
 * applications don't use traditional loops, but instead, they use recursion,
 * which has the same power as looping but in a package that lends itself
 * to immutable data and pure functions.
 *
 * In this module, you will explore using recursion to solve some of the same
 * tasks you would use looping to solve in a procedural programming language.
 */
package net.degoes

import zio.test._
import zio.test.TestAspect._

object Recursion extends ZIOSpecDefault {
  def spec =
    suite("Recursion") {
      suite("basics") {

        /**
         * EXERCISE
         *
         * Using recursion, compute the sum of a list of integers.
         */
        test("sum") {
          def sum(list: List[Int]): Int = {
            var res = 0
            for (ele <- list) res += ele
            res
          }

          assertTrue(sum(List(1, 2, 3, 4, 5)) == 15)
        } +
          /**
           * EXERCISE
           *
           * Using recursion, compute the maximum of a list of integers.
           */
          test("max") {
            def max(list: List[Int]): Int = {
              var max = 0
              list.foreach(ele => if (ele > max)  max = ele)
              max
            }

            assertTrue(max(List(1, 7, 3, 2, 4, 5)) == 7)
          } +
          /**
           * EXERCISE
           *
           * Using recursion, determine if a number is prime.
           */
          test("prime") {
            def isPrime(n: Int): Boolean = {
              for (i <- 2 to Math.sqrt(n))
                if (n % i == 0) return false
              true
            }

            assertTrue(!isPrime(4) && isPrime(7) && isPrime(11))
          } +
          /**
           * EXERCISE
           *
           * Using recursion, compute the nth fibonacci number. The fibonacci
           * sequence is given by, 0, 1, 1, <sum of two previous nums>...
           */
          test("fibs") {
            def fib(n: Int): Int = {
              if (n == 0) return 0
              else if (n == 1 || n == 2)  return 1
              else
                fib(n - 1) + fib(n - 2)
            }

            assertTrue(fib(3) == 2 && fib(4) == 3 && fib(5) == 5)
          } +
          /**
           * EXERCISE
           *
           * Using recursion, sort the provided list, by taking the head, and
           * sorting those less than the head, and those not less than the
           * head (separately), then concatenating them in the right order.
           */
          test("pivot sort") {
            def sort[A](list: List[A])(implicit ordering: Ordering[A]): List[A] = {
              for (i <- 0 to list.size - 1) {
                for (j <- i + 1 to list.size - 1) {
                  if (ordering.compare(list(i), list(j)) > 0) {
                    list.updated(i, list(j)).updated(j, list(i))
                  }
                }
              }
              list
            }

            assertTrue(sort(List(9, 23, 1, 5)) == List(1, 5, 9, 23))
          } +
          /**
           * EXERCISE
           *
           * Using recursion, implement a method to loop until a predicate is
           * satisfied.
           */
          test("loop") {
            def loop[S](start: S)(pred: S => Boolean)(iterate: S => S): S = {
              while (pred.apply(start)) iterate(start)
              start
            }

            val inc = loop(0)(_ < 10)(_ + 1)

            assertTrue(inc == 10)
          } +
          /**
           * EXERCISE
           *
           * Using recursion, implement a method that repeats the specified
           * action again and again, until the predicate is true.
           */
          test("repeat") {
            var input = "John" :: "Scotty" :: "Bob" :: "Sherlock" :: Nil

            val readLine: () => String = () =>
              input match {
                case Nil => ""
                case head :: tail =>
                  input = tail
                  head
              }

            def repeatWhile[A, S](action: () => A)(pred: A => Boolean)(reducer: (A, A) => A): A =
              while (pred.apply(action())) {
                reducer.apply()
              }

            val result = repeatWhile(readLine)(_ == "Sherlock")((a, b) => b)

            assertTrue(result == "Sherlock")
          } @@ ignore

      } +
        suite("tail recursion") {
           import scala.annotation.tailrec
          /**
           * EXERCISE
           *
           * Write a tail-recursive version of the previous `sum`.
           */
          test("sum") {
            @tailrec
            def sum(list: List[Int]): Int = list match {
               case Nil => 0
               case head :: tail => head + sum(tail)
             }

            assertTrue(sum(List(1, 2, 3, 4, 5)) == 15)
          } +
            /**
             * EXERCISE
             *
             * Write a tail-recursive version of the previous `max`.
             */
            test("max") {
              @tailrec
              def max(list: List[Int]): Int = list match {
                case Nil => -1
                case head :: tail =>
                  val tailMax: Int = max(tail)
                  if (tailMax > head) tailMax
                  else  head
              }

              assertTrue(max(List(1, 7, 3, 2, 4, 5)) == 7)
            } +
            /**
             * EXERCISE
             *
             * Write a tail-recursive version of the previous `loop`.
             */
            test("loop") {
              @tailrec
              def loop[S](start: S)(pred: S => Boolean)(iterate: S => S): S =
                if (pred.apply(start))
                  loop(iterate.apply(start))(pred)(iterate)
                else start

              val inc = loop(0)(_ < 10)(_ + 1)

              assertTrue(inc == 10)
            } +
            /**
             * EXERCISE
             *
             * Write a tail-recursive version of the previous `repeat`.
             */
            test("repeat") {
              var input = "John" :: "Scotty" :: "Bob" :: "Sherlock" :: Nil

              val readLine: () => String = () =>
                input match {
                  case Nil => ""
                  case head :: tail =>
                    input = tail
                    head
                }

              def repeatWhile[A, S](action: () => A)(pred: A => Boolean)(reducer: (A, A) => A): A = ???

              val result = repeatWhile(readLine)(_ == "Sherlock")((a, b) => b)

              assertTrue(result == "Sherlock")
            } @@ ignore +
            /**
             * EXERCISE
             *
             * Try to find a way to write the fib sequence using tail recursion.
             *
             * WARNING: Advanced.
             */
            test("fibs") {
              @tailrec
              def fib(n: Int): Int = n match {
                case 0 => 0
                case 1 || 2 => 1
                case other => fib(other - 1) + fib(other - 2)
              }

              assertTrue(fib(3) == 2 && fib(4) == 3 && fib(5) == 5)
            } +
            /**
             * EXERCISE
             *
             * Try to find a way to write the pivot sort using tail recursion.
             *
             * WARNING: Advanced.
             */
            test("pivot sort") {
              @tailrec
              def sort[A](list: List[A])(implicit ordering: Ordering[A]): List[A] = {
                if (list.isEmpty || list.length == 1) list
                else {
                  val pivot: A = list(list.length / 2)
                  val (lLst, sLst, rLst) = list.foldLeft{(List.empty[A], List.empty[A], List.empty[A])}{(total, x: A) =>
                    val (lessThan, same, greaterThan) = total
                    if (ordering.compare(x, pivot) < 0) (x::lessThan, same, greaterThan)
                    else if (ordering.compare(x, pivot) > 0) (lessThan, same, x::greaterThan)
                    else (lessThan, x::same, greaterThan)
                  }
                  sort(lLst) :: sLst :: sort(rLst)
                }
              }

              assertTrue(sort(List(9, 23, 1, 5)) == List(1, 5, 9, 23))
            }
        }
    }
}

/**
 * Recursion is the general-purpose replacement for looping in functional
 * Scala. While recursion should be avoided when simpler alternatives
 * exist (e.g. foldLeft on List), when not possible, recursion provides
 * a very powerful tool that can solve the most complex iterative problems.
 *
 * In this graduation project, you get to take a break and experiment with
 * recursion in a functional effect system as you implement a game of hangman.
 */
object RecursionGraduation extends zio.ZIOAppDefault {
  import zio._
  import java.io.IOException

  /**
   * EXERCISE
   *
   * Implement an effect that gets a single, lower-case character from
   * the user.
   */
  lazy val getChoice: ZIO[Any, IOException, Char] = ???


  /**
   * EXERCISE
   *
   * Implement an effect that prompts the user for their name, and
   * returns it.
   */
  lazy val getName: ZIO[Any, IOException, String] = ???

  /**
   * EXERCISE
   *
   * Implement an effect that chooses a random word from the dictionary.
   * The dictionary is `Dictionary`.
   */
  lazy val chooseWord: ZIO[Any, Nothing, String] = ???

  /**
   * EXERCISE
   *
   * Implement the main game loop, which gets choices from the user until
   * the game is won or lost.
   */
  def gameLoop(oldState: State): ZIO[Any, IOException, Unit] = ???

  def renderState(state: State): ZIO[Any, IOException, Unit] = {

    /**
     *
     *  f     n  c  t  o
     *  -  -  -  -  -  -  -
     *
     *  Guesses: a, z, y, x
     *
     */
    val word =
      state.word.toList
        .map(c => if (state.guesses.contains(c)) s" $c " else "   ")
        .mkString("")

    val line = List.fill(state.word.length)(" - ").mkString("")

    val guesses = " Guesses: " + state.guesses.mkString(", ")

    val text = word + "\n" + line + "\n\n" + guesses + "\n"

    Console.printLine(text)
  }

  final case class State(name: String, guesses: Set[Char], word: String) {
    final def failures: Int = (guesses -- word.toSet).size

    final def playerLost: Boolean = failures > 10

    final def playerWon: Boolean = (word.toSet -- guesses).isEmpty

    final def addChar(char: Char): State = copy(guesses = guesses + char)
  }

  final case class Step(output: String, keepPlaying: Boolean, state: State)

  def analyzeChoice(
    oldState: State,
    char: Char
  ): Step = {
    val newState = oldState.addChar(char)

    if (oldState.guesses.contains(char))
      Step("You already guessed this character!", true, newState)
    else if (newState.playerWon)
      Step("Congratulations, you won!!!", false, newState)
    else if (newState.playerLost)
      Step(s"Sorry, ${oldState.name}, you lost. Try again soon!", false, newState)
    else if (oldState.word.contains(char))
      Step(s"Good work, ${oldState.name}, you got that right! Keep going!!!", true, newState)
    else Step(s"Uh, oh! That choice is not correct. Keep trying!", true, newState)
  }

  /**
   * EXERCISE
   *
   * Execute the main function and verify your program works as intended.
   */
  def run =
    for {
      name  <- getName
      word  <- chooseWord
      state = State(name, Set(), word)
      _     <- renderState(state)
      _     <- gameLoop(state)
    } yield ()
}
