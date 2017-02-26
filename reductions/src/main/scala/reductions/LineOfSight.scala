package reductions

import org.scalameter._
import common._

object LineOfSightRunner {
  
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}

object LineOfSight {

  def max(a: Float, b: Float): Float = if (a > b) a else b

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    input.zipWithIndex.foreach{
      case (height: Float, i: Int) => { output(i) = if (i == 0) i else max(output.max, height / i) }
    }
  }

  sealed abstract class Tree {
    def maxPrevious: Float
  }

  case class Node(left: Tree, right: Tree) extends Tree {
    val maxPrevious = max(left.maxPrevious, right.maxPrevious)
  }

  case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

  /** Traverses the specified part of the array and returns the maximum angle.
   */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    var max = 0f
    for (i <- from until until) {
      val tangent = if (i == 0) i else input(i) / i
      if (tangent > max) max = tangent
    }
    max
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
  def upsweep(input: Array[Float], from: Int, end: Int, threshold: Int): Tree = {
    if ((end - from) <= threshold) Leaf(from, end, upsweepSequential(input, from, end))
    else {
      val middle = (end + from) / 2
      val (left, right) = parallel(upsweep(input, from, middle, threshold), upsweep(input, middle, end, threshold))
      Node(left, right)
    }
  }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
  def downsweepSequential(input: Array[Float], output: Array[Float], startingAngle: Float, from: Int, until: Int): Unit = {
    var max = startingAngle
    for (i <- from until until) {
      val tangent = if (i == 0) i else input(i) / i
      if (tangent > max) max = tangent
      output(i) = max
    }
  }

  def downsweepTraverse(input: Array[Float], output: Array[Float], idx: Int, until: Int, startingAngle: Float): Unit = {
    if (idx == until) return
    else {
      val newAngle = if (idx == 0) 0 else max(input(idx) / idx, startingAngle)
      output(idx) = newAngle
      downsweepTraverse(input, output, idx + 1, until, newAngle)
    }
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepTraverse` to write
   *  the `output` angles.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float, tree: Tree): Unit = {
    tree match {
      case Leaf(from, until, _) => downsweepTraverse(input, output, from, until, startingAngle)
      case Node(left, right) => parallel(
        downsweep(input, output, right.maxPrevious, left),
        downsweep(input, output, left.maxPrevious, right)
      )
    }
  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float], threshold: Int): Unit = {
    val reductionTree = upsweep(input, 0, input.length - 1, threshold)
    downsweep(input, output, 0, reductionTree)
    val lastInputIdx: Int = input.length - 1
    output(lastInputIdx) = max(output(lastInputIdx - 1), input(lastInputIdx) / lastInputIdx)
  }
}
