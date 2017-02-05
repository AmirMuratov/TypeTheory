package library

import org.parboiled2.{Rule1, _}

import scala.util.{Failure, Success}


/**
  * Created by amir on 04.02.17.
  */
class TermParser(val input: ParserInput) extends Parser {

  def TermEq = rule {
    TermEqual ~ EOI
  }

  def TermEqual: Rule1[(Term, Term)] = rule {
    TermP ~ "=" ~ TermP ~> ((x: Term, y: Term) => (x, y))
  }

  def TermP: Rule1[Term] = rule {
    Func | Varr
  }

  def Func: Rule1[Term] = rule {
    FuncName ~ "(" ~ oneOrMore(TermP).separatedBy(",") ~ ")" ~> Function
  }

  def Varr: Rule1[Term] = rule {
    VarName ~> Variable
  }

  def FuncName: Rule1[String] = rule {
    capture(anyOf("abcdefgh") ~ zeroOrMore(CharPredicate.LowerAlpha | CharPredicate.Digit | '''))
  }

  def VarName: Rule1[String] = rule {
    capture(anyOf("ijklmnopqrstuvwxyz") ~ zeroOrMore(CharPredicate.LowerAlpha | CharPredicate.Digit | '''))
  }
}

object TermParser {
  def parseExpression(s: String): Option[(Term, Term)] = {
    val parser = new TermParser(s)
    parser.TermEq.run() match {
      case Success(result) => Some(result)
      case Failure(e: ParseError) =>
        println("Expression is not valid: " + parser.formatError(e))
        None
      case _ => None
    }
  }
}

