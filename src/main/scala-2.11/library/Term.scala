package library

/**
  * Created by amir on 04.02.17.
  */
sealed trait Term {
  override def toString: String = {
    this match {
      case Function(name, args) => name + "(" + args.mkString(",") + ")"
      case Variable(name) => name
    }
  }

  def ==(o: Term): Boolean = {
    (this, o) match {
      case (Variable(name1), Variable(name2)) if name1.equals(name2) => true
      case (Function(name1, args1), Function(name2, args2)) if name1.equals(name2)
        && args1.zip(args2).forall((x) => x._1 == x._2) => true
      case _ => false
    }
  }
}

case class Function(name: String, args: Seq[Term]) extends Term

case class Variable(name: String) extends Term