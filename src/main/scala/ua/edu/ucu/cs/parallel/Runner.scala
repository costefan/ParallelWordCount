package ua.edu.ucu.cs.parallel

import scala.io.Source

object Runner {

  trait Monoid[A] {
    def op(x: A, y: A): A
    def zero: A
  }

  def foldMapBalanced[A, B](xs: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (xs.isEmpty)
      m.zero
    else if (xs.length == 1)
      f(xs(0))
    else {
      val (l, r) = xs.splitAt(xs.length / 2)
      m.op(
        foldMapBalanced(l, m)(f),
        foldMapBalanced(r, m)(f)
      )
    }

  def foldSegment[A](xs: IndexedSeq[A],
                     from: Int, to: Int, m: Monoid[A]): A = {
    var res = xs(from)
    var index = from + 1
    while (index < to) {
      res = m.op(res, xs(index))
      index = index + 1
    }

    res
  }

  def foldPar[A](xs: IndexedSeq[A],
                 from: Int, to: Int, m: Monoid[A])
                (implicit thresholdSize: Int): A =
    if (to - from < thresholdSize)
      foldSegment(xs, from, to, m)
    else {
      val middle = from + (to - from) / 2
      val (l, r) = parallel(
        foldPar(xs, from, middle, m)(thresholdSize),
        foldPar(xs, middle, to, m)(thresholdSize)
      )
      m.op(l, r)
    }

  def foldMapSegment[A, B](xs: IndexedSeq[A],
                           from: Int, to: Int,
                           m: Monoid[B])
                          (f: A => B): B = {
    var res = f(xs(from))
    var index = from + 1
    while (index < to) {
      res = m.op(res, f(xs(index)))
      index = index + 1

    }
    res
  }

  def foldMapPar[A, B](xs: IndexedSeq[A],
                       from: Int, to: Int,
                       m: Monoid[B])(f: A => B)
                      (implicit thresholdSize: Int): B =
    if (to - from <= thresholdSize)
      foldMapSegment(xs, from, to, m)(f)
    else {
      val middle = from + (to - from) / 2
      val (l, r) = parallel(
        foldMapPar(xs, from, middle, m)(f)(thresholdSize),
        foldMapPar(xs, middle, to, m)(f)(thresholdSize)
      )
      m.op(l, r)
    }

  val monoid = new Monoid[(String, Int, String)] {
    override def op(x: (String, Int, String),
                    y: (String, Int, String)): (String, Int, String) = {
      if (x._1 == "")
        (y._1, 0, "")
      else if (y._1 == " ")
        if ((x._3 != " ") && (x._3 != ""))
          (x._1, x._2 + 1, y._1)
        else
          (x._1, x._2, y._1)
      else if (x._3 == " ")
        (x._1, x._2, y._1)
      else
        if (x._3 == "")
          (x._1 + y._1, x._2 + y._2, x._3 + y._3)
        else (x._1, x._2 + y._2, x._3 + y._1)



    }

    override def zero: (String, Int, String) = ("", 0, "")
  }

  def calculateLineRes(): (String, Int, String) = {
    def f = (x: Char) => (x.toString, 0, "")
    var index: Int = 0
    var res = ("", 0, "")

    val bufferedSource = Source.fromFile("file.txt")

    for (line <- bufferedSource.getLines) {
      if (index == 0)
        res = foldMapSegment(line, 0, line.length, monoid)(f)
      else
        res = monoid.op(res, foldMapSegment(line, 0, line.length, monoid)(f))

      index = index + 1
    }

    res
  }

  def main(args: Array[String]): Unit = {
    implicit val thresholdSize: Int = 10
    val res = calculateLineRes()
    println(res)
  }
}
