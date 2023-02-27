package chap2

import scala.annotation.tailrec

object Exerc {

  object `2.1` {
    def fib(n: Int): Int = {

      @tailrec
      def loop(idx: Int, prev: Int, current: Int): Int = {
        if (idx == 0) current
        else loop(idx - 1, current, current + prev)
      }

      loop(n - 2, 0, 1)
    }
  }

  object `2.2` {
    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
      as.sortWith(ordered) sameElements as
  }

  def partial[A, B, C](a: A, f: (A, B) => C): B => C = f(a, _) // simply beautiful </3

  object `2.3` {
    def curry[A, B, C](a: A, f: (A, B) => C): A => (B => C) = a => b => f(a, b) // beautiful AF
  }

  object `2.4` {
    def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = f(_)(_) // wtf :D
  }

  object `2.5` {
    def compose[A, B, C](f: A => B)(b: B => C): A => C = b compose f
  }

  def main(args: Array[String]): Unit = {
    println(`2.1`.fib(5)) // 3
    println(`2.2`.isSorted[Int](Array(1, 2, 3), _ < _)) // true
    println(`2.3`.curry[Int, String, Boolean](5, (i, s) => i.toString == s)(10)("10")) // true
    println(`2.4`.uncurry[Int, String, Boolean](i => s => (i.toString == s))(10, "10")) // true
    println(`2.5`.compose[Int, String, Boolean](_.toString)(_.isEmpty)(50)) // false
  }

}
