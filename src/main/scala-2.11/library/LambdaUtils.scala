package library

/**
  * Created by amir on 03.02.17.
  */

object LambdaUtils {
  def getFreeVars(expr: LambdaExpr): Set[String] = {
    getFreeVars(expr, Set())
  }

  def getFreeVars(expr: LambdaExpr, notFree: Set[String]): Set[String] = {
    expr match {
      case Lambda(varName, x) => getFreeVars(x, notFree + varName)
      case App(x, y) => getFreeVars(x, notFree) ++ getFreeVars(y, notFree)
      case Var(name) => if (!notFree.contains(name)) Set(name) else Set()
      case _ => throw new IllegalStateException()
    }
  }

  def makeSubst(expr: LambdaExpr, varName: String, exprToSubst: LambdaExpr): Option[LambdaExpr] = {
    subst(expr, varName, exprToSubst, Set())
  }

  def subst(expr: LambdaExpr, varName: String, exprToSubst: LambdaExpr, notFree: Set[String]): Option[LambdaExpr] = {
    expr match {
      case l@Lambda(name, x) => if (!name.equals(varName)) {
        val f = subst(x, varName, exprToSubst, notFree + name)
        if (f.isDefined)
          Some(Lambda(name, f.get))
        else
          None
      }
      else
        Some(l)
      case App(x, y) =>
        val f = subst(x, varName, exprToSubst, notFree)
        val s = subst(y, varName, exprToSubst, notFree)
        if (f.isDefined && s.isDefined)
          Some(App(f.get, s.get))
        else
          None
      case v@Var(name) => if (varName.equals(name)) {
        if (notFree.intersect(getFreeVars(exprToSubst)).isEmpty)
          Some(exprToSubst)
        else
          None
      }
      else
        Some(v)
      case _ => throw new IllegalStateException()
    }
  }

  //(\x.x x) (\x.x x)
}
