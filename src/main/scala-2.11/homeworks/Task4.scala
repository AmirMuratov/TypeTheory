package homeworks

import java.io.PrintWriter

import library.{LambdaExprParser, NormalForm}

import scala.io.Source.fromFile

/**
  * Created by amir on 03.02.17.
  */
object Task4 {
  def main(args: Array[String]): Unit = {
    val inputFileName = "test.in"
    val outputFileName = "res.out"
    val output = new PrintWriter(outputFileName)
    val expressions = fromFile(inputFileName).getLines().filter(!_.isEmpty).map(LambdaExprParser.parseExpression).toSeq
    if (expressions.exists(_.isEmpty)) {
      println("Can't parse some expressions")
      return
    }
    println("parsing done")
    expressions.map(_.get).foreach((x) => {
      val t = new NormalForm()
      output.println(t.findNormalForm(x))
      //t.memory.foreach(println)
    })
    output.close()
  }
}
