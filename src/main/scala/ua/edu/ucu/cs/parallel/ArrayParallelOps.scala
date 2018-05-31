

package ua.edu.ucu.cs.parallel

object ArrayParallelOps {
  def power(x: Int, p: Double): Int = math.exp(p * math.log(Math.abs(x))).toInt

  def mapArraySegment[A, B](
                             source: Array[A],
                             from: Int, to: Int,
                             f: A => B,
                             target: Array[B]) {

    var index = from
    while (index < to) {
      target(index) = f(source(index))
      index = index + 1
    }
  }

  val threshold = 1000

  def mapArraySegmentPar[A, B](
                                source: Array[A],
                                from: Int, to: Int,
                                f: A => B,
                                target: Array[B]) {
    if (to - from < threshold) mapArraySegment(source, from, to, f, target)
    else {
      val middle = from + (to - from) / 2
      val (l, r) = parallel(mapArraySegmentPar(source, from, middle, f, target),
        mapArraySegmentPar(source, middle, to, f, target))
    }
  }


  def main(args: Array[String]): Unit = {

  }
}