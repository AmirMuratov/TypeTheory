package homeworks

import java.io.PrintWriter

import library.LambdaExprParser
import library.LambdaUtils.getFreeVars

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

    expressions.map((x) => getFreeVars(x.get)).foreach((x) => x.sorted.foreach(output.println))
    output.close()
  }
}
