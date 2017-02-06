package library.task6

import library._
import library.terms.{Term, Variable, Function}
import library.terms.TermUtils.unificate

import scala.collection.mutable

/**
  * Created by amir on 06.02.17.
  */
class TypeDefiner {
  def getType(expr: LambdaExpr): (Type, mutable.HashMap[String, Type]) = {
    val res = getEqSystemAndType(expr, Map())
    val substitution = unificate(res._1.map((x) => (typeToTerm(x._1), typeToTerm(x._2)))).
      map((x) => (termToType(x._1), termToType(x._2)))
    val context = definedFreeVars.map((x) => {
      val t = substitution.find((y) => y._1 == x._2)
      if (t.isDefined)
        (x._1, t.get._2)
      else
        x
    })
    (substitution.foldLeft(res._2)((a, b) => b match {case (SType(name), x) => a.subst(name, x)}),
    context)
  }

  val definedFreeVars: mutable.HashMap[String, Type] = new mutable.HashMap()
  var varNum: Int = 0
  def getNewType(): Type = {
    varNum += 1
    SType("a" + varNum.toString)
  }

  def getEqSystemAndType(expr: LambdaExpr, notFree: Map[String, Type]): (Seq[(Type, Type)], Type) = {
    expr match {
      case Var(name) if notFree.contains(name) => (Seq(), notFree(name))
      case Var(name) if definedFreeVars.contains(name) => (Seq(), definedFreeVars(name))
      case Var(name) =>
        val t = getNewType()
        definedFreeVars.put(name, t)
        (Seq(), t)
      case App(expr1, expr2) =>
        val x1 = getEqSystemAndType(expr1, notFree)
        val x2 = getEqSystemAndType(expr2, notFree)
        val newT = getNewType()
        (x1._1 ++ x2._1 ++ Seq((x1._2, Arrow(x2._2, newT))), newT)
      case Lambda(name, expr1) =>
        val newT = getNewType()
        val x1 = getEqSystemAndType(expr1, notFree + (name -> newT))
        (x1._1, Arrow(newT, x1._2))
    }
  }

  def typeToTerm(t: Type): Term = {
    t match {
      case SType(name) => Variable(name)
      case Arrow(t1, t2) => Function("Arrow", Seq(typeToTerm(t1), typeToTerm(t2)))
    }
  }
  def termToType(t: Term): Type = {
    t match {
      case Variable(name) => SType(name)
      case Function(name, t1 :: t2 :: Nil) if "Arrow".equals(name) => Arrow(termToType(t1), termToType(t2))
      case _ => throw new IllegalArgumentException
    }
  }

}
