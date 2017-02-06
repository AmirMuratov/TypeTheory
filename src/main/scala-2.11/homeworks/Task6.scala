package homeworks

import java.io.PrintWriter

import library.LambdaExprParser
import library.task6.TypeDefiner

import scala.io.Source.fromFile

/**
  * Created by amir on 04.02.17.
  */
object Task6 {
  def main(args: Array[String]): Unit = {
    val inputFileName = "test.in"
    val outputFileName = "res.out"
    val output = new PrintWriter(outputFileName)

    val expressions = fromFile(inputFileName).getLines().filter(!_.isEmpty).map(LambdaExprParser.parseExpression).toSeq
    if (expressions.exists(_.isEmpty)) {
      println("Can't parse some expressions")
      return
    }
    val expr = expressions.head.get
    try {
      val (ansType, ansContext) = (new TypeDefiner).getType(expr)
      println(ansType)
      ansContext.foreach((x) => println(x._1 + ":" + x._2))
      output.println(ansType)
      ansContext.foreach((x) => output.println(x._1 + ":" + x._2))
    } catch {
      case _: Exception => output.println("Лямбда-выражение не имеет типа.")
    }
    output.close()
  }
}
