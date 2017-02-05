package library

import library.LambdaUtils._
import scala.collection.mutable

/**
  * Created by amir on 04.02.17.
  */
class NormalForm {
  val memory = new mutable.HashMap[LambdaExpr, LambdaExpr]()

  def oneReduction(expr: LambdaExpr): LambdaExpr = {
    expr match {
      case a@App(Lambda(name, x), y) =>
        val s = makeSubst(x, name, y)
        if (s.isDefined)
          s.get
        else
          a
      case App(x, y) =>
        val xnf = oneReduction(x)
        if (xnf == x)
          App(x, oneReduction(y))
        else
          App(xnf, y)
      case Lambda(name, x) => Lambda(name, oneReduction(x))
      case a@Var(_) => a
    }
  }

  def findNormalForm(expr: LambdaExpr): LambdaExpr = {
    if (memory.contains(expr))
      return memory(expr)
    //println(expr)
    val nf = expr match {
      case a@App(Lambda(name, x), y) =>
        val s = makeSubst(x, name, y)
        if (s.isDefined)
          findNormalForm(s.get)
        else
          a
      case App(x, y) =>
        val xnf = oneReduction(x)
        if (xnf == x)
          App(x, findNormalForm(y))
        else
          findNormalForm(App(xnf, y))
      case Lambda(name, x) => Lambda(name, findNormalForm(x))
      case a@Var(_) => a
    }
    memory.put(expr, nf)
    nf
  }

}
