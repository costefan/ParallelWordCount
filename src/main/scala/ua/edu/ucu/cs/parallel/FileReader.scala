package ua.edu.ucu.cs.parallel
import scala.io.Source


object FileReader {
  def readFile(fileName: String): String = {
    val fileContents = Source.fromFile(fileName).getLines.mkString
    fileContents
  }

  def readFileIterator(fileName: String): Iterator[String] = {
    val bufferedSource = Source.fromFile(fileName)
    bufferedSource.getLines
  }
}
