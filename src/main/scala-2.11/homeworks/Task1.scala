package homeworks

import java.io.PrintWriter

import library.LambdaExprParser

import scala.io.Source._

/**
  * @author amir.
  */
object Task1 {
  def main(args: Array[String]): Unit = {
    val inputFileName = "test.in"
    val outputFileName = "res.out"
    val output = new PrintWriter(outputFileName)

    val expressions = fromFile(inputFileName).getLines().filter(!_.isEmpty).map(LambdaExprParser.parseExpression).toSeq
    if (expressions.exists(_.isEmpty)) {
      println("Can't parse some expressions")
      return
    }

    expressions.map(_.get).foreach(output.println)
    output.close()
  }
}
