package library.task6

/**
  * Created by amir on 06.02.17.
  */
sealed trait Type {
  override def toString: String = {
    this match {
      case SType(name) => name
      case Arrow(x, y) => "(" + x.toString + ") -> (" + y.toString + ")"
    }
  }

  def subst(name: String, t: Type): Type = {
    this match {
      case Arrow(x, y) => Arrow(x.subst(name, t), y.subst(name, t))
      case a@SType(name1) => if (name1.equals(name)) t else a
    }
  }
}

case class Arrow(from: Type, to: Type) extends Type

case class SType(name: String) extends Type