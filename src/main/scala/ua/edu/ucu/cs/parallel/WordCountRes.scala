package ua.edu.ucu.cs.parallel

trait CharSeq {

}

case object EmptyChar extends CharSeq
case class NonEmptyChar() extends CharSeq {

}
