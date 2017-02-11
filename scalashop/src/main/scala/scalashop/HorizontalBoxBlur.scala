package scalashop

import org.scalameter._
import common._

import scala.collection.parallel.Task

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    var xIndex = 0
    var yIndex = from
    while (yIndex < end) {
      while (xIndex < src.width) {
        dst.update(xIndex, yIndex, scalashop.boxBlurKernel(src, xIndex, yIndex, radius))
        xIndex += 1
      }
      xIndex = 0
      yIndex += 1
    }
  }

  def parBlurRanges(src: Img, numTasks: Int): List[(Int, Int)] = {
    val increments = if (src.height / numTasks == 0) 1 else src.height / numTasks
    val splittingPoints: List[Int] = (0 to src.height by increments).toList
    val stripeUpperBounds =
      if (src.height % numTasks == 0) splittingPoints.tail
      else List.concat(splittingPoints.tail, List(src.height))
    splittingPoints.zip(stripeUpperBounds)
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val ranges = this.parBlurRanges(src, numTasks)
    val tasks = ranges.map((range: (Int, Int)) => task {
      this.blur(src, dst, range._1, range._2, radius)
    })
    tasks.foreach((task) => task.join())
  }
}
