package library

/**
  * @author amir.
  */
sealed trait LambdaExpr {
  override def toString: String = {
    this match {
      case Lambda(varName, expr) => "(\\" + varName + "." + expr.toString + ")"
      case App(expr1, expr2) => "(" + expr1.toString + " " + expr2.toString + ")"
      case Var(varName) => varName
      case _ => throw new IllegalStateException()
    }
  }

  def ==(o: LambdaExpr): Boolean = {
    LambdaExpr.equalsWithSubst(this, Map(), o, Map())
  }

  override def hashCode(): Int = hashCodeWithSubst(Map())

  def hashCodeWithSubst(subst: Map[String, Int]):Int = {
    this match {
      case App(x, y) => x.hashCodeWithSubst(subst) * 17 + y.hashCodeWithSubst(subst) * 23
      case Lambda(name, expr) => 47 * expr.hashCodeWithSubst(subst + (name -> subst.size))
      case Var(name) => if (subst.contains(name)) subst(name) * 41 else name.hashCode * 41
    }
  }

}

case class Lambda(varName: String, expr: LambdaExpr) extends LambdaExpr

case class App(expr1: LambdaExpr, expr2: LambdaExpr) extends LambdaExpr

case class Var(name: String) extends LambdaExpr

object LambdaExpr {
  def equalsWithSubst(l: LambdaExpr, lSubst: Map[String, Int],
                      r: LambdaExpr, rSubst: Map[String, Int]): Boolean = {
    (l, r) match {
      case (App(x1, y1), App(x2, y2)) =>
        equalsWithSubst(x1, lSubst, x2, rSubst) && equalsWithSubst(y1, lSubst, y2, rSubst)
      case (Lambda(name1, expr1), Lambda(name2, expr2)) =>
        equalsWithSubst(expr1, lSubst + (name1 -> lSubst.size), expr2, rSubst + (name2 -> rSubst.size))
      case (Var(name1), Var(name2)) =>
        lSubst.contains(name1) && rSubst.contains(name2) && lSubst.get(name1) == rSubst.get(name2) ||
          !lSubst.contains(name1) && !rSubst.contains(name2) && name1.equals(name2)
      case (_, _) => false
    }
  }
}
