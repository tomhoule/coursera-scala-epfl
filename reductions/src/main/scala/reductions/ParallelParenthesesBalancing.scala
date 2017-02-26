package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    balanceRec(chars, 0)
  }

  def balanceRec(chars: Array[Char], openParens: Int): Boolean =  {
    if (chars.isEmpty) openParens == 0
    else if (chars.head == '(') balanceRec(chars.tail, openParens + 1)
    else if (chars.head == ')') if (openParens == 0) false else balanceRec(chars.tail, openParens - 1)
    else balanceRec(chars.tail, openParens)
  }

  def combine(first_segment: (Int, Int), second_segment: (Int, Int)): (Int, Int) = {
    (first_segment._1, (first_segment._2 - second_segment._1) + second_segment._2)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, leadingClosingParens: Int, openParens: Int): (Int, Int) = {
      if (idx == until) (leadingClosingParens, openParens)
      else {
        chars(idx) match {
          case ')' => if (openParens > 0) traverse(idx + 1, until, leadingClosingParens, openParens - 1)
                      else traverse(idx + 1, until, leadingClosingParens + 1, openParens);
          case '(' => traverse(idx + 1, until, leadingClosingParens, openParens + 1)
          case _ => traverse(idx + 1, until, leadingClosingParens, openParens)
        }
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val middle = (until + from) / 2
        val (left, right) = parallel(reduce(from, middle), reduce(middle, until))
        combine(left, right)
      }
    }
    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
