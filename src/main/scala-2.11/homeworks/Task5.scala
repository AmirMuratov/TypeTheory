package homeworks

import java.io.PrintWriter

import library.TermParser
import library.TermUtils.unificate

import scala.io.Source.fromFile

/**
  * Created by amir on 04.02.17.
  */
object Task5 {
  def main(args: Array[String]): Unit = {
    val inputFileName = "tests/HW5/nosolution1.in"
    val outputFileName = "res.out"
    val output = new PrintWriter(outputFileName)

    val expressions = fromFile(inputFileName).getLines().filter(!_.isEmpty).map(TermParser.parseExpression).toSeq
    if (expressions.exists(_.isEmpty)) {
      println("Can't parse some expressions")
      return
    }
    try {
      unificate(expressions.map(_.get)).foreach((x) => output.println(x._1 + "=" + x._2))
    } catch {
      case _: IllegalStateException => output.println("Система не совместна")
    }
    output.close()
  }
}
