package library.terms

/**
  * Created by amir on 05.02.17.
  */
object TermUtils {

  // checks if variable name contains in Term term
  def contains(name: String, term: Term): Boolean = {
    term match {
      case Variable(name1) if name.equals(name1) => true
      case Function(_, args) if args.exists(contains(name, _)) => true
      case _ => false
    }
  }

  def isInconsistent(termEq: (Term, Term)): Boolean = {
    termEq match {
      case (Function(name1, _), Function(name2, _)) if !name1.equals(name2) => true
      case (Variable(name), a@Function(_, _)) if contains(name, a) => true
      case _ => false
    }
  }

  def isInconsistent(terms: Seq[(Term, Term)]): Boolean = {
    terms.exists(isInconsistent)
  }


  def substitute(varName: String, term: Term, subst: Term): Term = {
    term match {
      case Function(name1, args) => Function(name1, args.map(substitute(varName, _, subst)))
      case a@Variable(name) => if (varName.equals(name)) subst else a
    }
  }

  def transform(termEq: (Term, Term)): Seq[(Term, Term)] = {
    termEq match {
      case (a1@Function(_, _), a2@Variable(_)) => Seq((a2, a1))
      case (Function(name1, args1), Function(name2, args2)) if name1.equals(name2) =>
        args1.zip(args2)
      case x => Seq(x)
    }
  }

  def deleteUseless(terms: Seq[(Term, Term)]): Seq[(Term, Term)] = {
    terms.filter((x) => !(x._1 == x._2))
  }

  def deleteDuplicates(terms: Seq[(Term, Term)]): Seq[(Term, Term)] = {
    terms.toSet.toSeq
  }
  def subst(terms: Seq[(Term, Term)]): Seq[(Term, Term)] = {
    val termToSubst = terms.find {
      case (Variable(name), _) if
      terms.count((x) => contains(name, x._1) || contains(name, x._2)) > 1 => true
      case _ => false
    }
    if (termToSubst.isEmpty)
      return terms
    //println("subst: " + termToSubst.get._1 + "=" + termToSubst.get._2)
    termToSubst.get match {
      case (Variable(name), s) => terms.map((x) => if (x == termToSubst.get) x else
        (substitute(name, x._1, s), substitute(name, x._2, s)))
    }
  }

  def isSolved(terms: Seq[(Term, Term)]): Boolean = {
    if (terms.forall {
      case (Variable(_), _) => true
      case _ => false
    }) {
      val vars = terms.map(_._1)
      val values = terms.map(_._2)
      return vars.toSet.size == vars.size && !vars.exists((x) => values.exists(contains(x.toString, _)))
    }
    false
  }

  def unificate(terms: Seq[(Term, Term)]): Seq[(Term, Term)] = {
    var t = terms
    while (!isSolved(t)) {
      //println("==before==")
      //t.foreach((x) => println(x._1 + "=" + x._2))
      //println("====")
      if (isInconsistent(t))
        throw new IllegalStateException
      t = subst(t)
      //println("==afterSubst==")
      //t.foreach((x) => println(x._1 + "=" + x._2))
      //println("====")
      t = t.flatMap(transform)
      //println("==afterTransform==")
      //foreach((x) => println(x._1 + "=" + x._2))
      //println("====")
      t = deleteUseless(t)
      //println("==afterDeleteUsless==")
      //t.foreach((x) => println(x._1 + "=" + x._2))
      //println("====")
      t = deleteDuplicates(t)
    }
    t
  }
}
