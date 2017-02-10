package library

import org.parboiled2._
import scala.util.{Failure, Success}

/**
  * @author amir.
  */
class LambdaExprParser(val input: ParserInput) extends Parser {

  def ExprWithSubst = rule {
    WithSubst ~ EOI
  }

  def Expression = rule {
    LambdaExp ~ EOI
  }

  def WithSubst: Rule1[(LambdaExpr, String, LambdaExpr)] = rule {
    (LambdaExp ~ "[" ~ VarName ~ ":=" ~ LambdaExp ~ "]") ~> ((x: LambdaExpr, y: String, z: LambdaExpr) => (x, y, z))
  }

  def LambdaExp: Rule1[LambdaExpr] = rule {
    Appl
    //(optional(Appl ~ " ") ~ LambdaP) ~>
    //  ((f: Option[LambdaExpr], s: LambdaExpr) => if (f.isDefined) App(f.get, s) else s) | Appl
  }


  def Appl: Rule1[LambdaExpr] = rule {
    Atom ~ zeroOrMore(" " ~ Atom ~> App) //oneOrMore(" " ~ Atom ~> App)
  }

  def Atom: Rule1[LambdaExpr] = rule {
    Varr | ("(" ~ LambdaExp ~ ")") | LambdaP
  }

  def LambdaP: Rule1[LambdaExpr] = rule {
    "\\" ~ VarName ~ "." ~ LambdaExp ~> Lambda
  }

  def Varr: Rule1[Var] = rule {
    VarName ~> Var
  }

  def VarName: Rule1[String] = rule {
    capture(CharPredicate.LowerAlpha ~ zeroOrMore(CharPredicate.LowerAlpha | CharPredicate.Digit | '''))
  }
}

object LambdaExprParser {
  def parseExprWithSubst(s: String): Option[(LambdaExpr, String, LambdaExpr)] = {
    val parser = new LambdaExprParser(s)
    parser.ExprWithSubst.run() match {
      case Success(result) => Some(result)
      case Failure(e: ParseError) =>
        println("Expression is not valid: " + parser.formatError(e))
        None
      case _ => None
    }
  }

  def parseExpression(s: String): Option[LambdaExpr] = {
    val parser = new LambdaExprParser(s)
    parser.Expression.run() match {
      case Success(result) => Some(result)
      case Failure(e: ParseError) =>
        println("Expression is not valid: " + parser.formatError(e))
        None
      case _ => None
    }
  }
}

