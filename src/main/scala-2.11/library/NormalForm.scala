package library

import library.LambdaUtils._
import scala.collection.mutable

/**
  * Created by amir on 04.02.17.
  */
class NormalForm {

  private val memory = mutable.HashMap.empty[(String, LambdaExpr, LambdaExpr), LambdaExpr]
  private val vars = mutable.Map.empty[String, Var]

  def substitute(lam: LambdaExpr, v: String, e: LambdaExpr, scope: Set[String] = Set()): LambdaExpr = lam match {
    case App(x, y) =>
      App(substitute(x, v, e, scope), substitute(y, v, e, scope))
    case t@Lambda(name, body) =>
      val bodyFree = getFreeVars(body)
      if (name == v || !bodyFree.contains(v)) {
        t
      } else {
        val free = getFreeName(bodyFree ++ getFreeVars(e) ++ scope)
        val freeVar = vars.get(free) match {
          case Some(oldV) => oldV
          case None =>
            val freeVar = Var(free)
            vars(free) = freeVar
            freeVar
        }
        val sBody = substitute(substitute(body, name, freeVar, scope + free), v, e, scope + free)
        if ((freeVar.name eq name) && (sBody eq body)) lam else Lambda(freeVar.name, sBody)
      }
    case Var(name) if v == name => e
    case a@Var(_) => a
  }

  def getFreeName(used: Set[String], range: Seq[String] = (1 to 30).map("a" + _.toString)): String = {
    range.find(c => !used.contains(c)) match {
      case Some(c) => c
      case _ => getFreeName(used, range.map(c => c + "'"))
    }
  }

  def headNormalForm(expr: LambdaExpr, notFree: Set[String] = Set()): LambdaExpr = {
    expr match {
      case Lambda(name, x) => Lambda(name, headNormalForm(x, notFree + name))
      case App(x, y) =>
        val xhnf = headNormalForm(x, notFree)
        xhnf match {
          case Lambda(name, body) =>
            val k = (name, body, y)
            memory.get(k) match {
              case Some(r) => r
              case None =>
                val hnf = headNormalForm(substitute(body, name, y, notFree + name), notFree + name)
                memory(k) = hnf
                hnf
            }
          case c => App(c, y)
        }
      case v: Var => v
    }
  }

  def normalForm(expr: LambdaExpr, notFree: Set[String] = Set()): LambdaExpr = {
    expr match {
      case Lambda(name, x) =>
        Lambda(name, normalForm(x, notFree + name))
      case App(x, y) =>
        headNormalForm(x) match {
          case Lambda(name, body) => normalForm(substitute(body, name, y, notFree + name), notFree + name)
          case o => App(normalForm(o), normalForm(y))
        }
      case a@Var(_) => a
    }
  }
}
