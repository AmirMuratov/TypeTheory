package homeworks

import java.io.PrintWriter

import library.LambdaExprParser
import library.LambdaUtils.makeSubst

import scala.io.Source.fromFile

/**
  * Created by amir on 03.02.17.
  */
object Task3 {
  def main(args: Array[String]): Unit = {
    val inputFileName = "test.in"
    val outputFileName = "res.out"
    val output = new PrintWriter(outputFileName)

    val expressions = fromFile(inputFileName).getLines().filter(!_.isEmpty).map(LambdaExprParser.parseExprWithSubst).toSeq
    if (expressions.exists(_.isEmpty)) {
      println("Can't parse some expressions")
      return
    }

    expressions.map(_.get).map((x) => makeSubst(x._1, x._2, x._3)).
      foreach((x) => if (x.isDefined) output.println(x.get)
      else output.println("Нет свободы для подстановки для переменной"))
    output.close()
  }
}
