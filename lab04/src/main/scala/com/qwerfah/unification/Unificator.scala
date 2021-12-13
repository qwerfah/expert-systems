package com.qwerfah.unification

case class Substitution(raw: (Term, Variable)):
  def t1 = raw._1
  def t2 = raw._2

  def apply(term: Term): Term = term match
    case Variable(_) if term == raw._2 => raw._1
    case Expression(_, terms*) =>
      val applied = terms.map(apply(_))
      Expression(applied.mkString, applied: _*)
    case _ => term

/** Abstract term set unificator. */
trait Unificator:
  def apply(terms: Seq[Term]): Term
  def compose(other: Unificator): Unificator

/** Represent standart term set unificator - collection of substitutions, where each element is a
  * pair of terms.
  */
case class SeqUnificator(substitutions: Substitution*) extends Unificator:
  override def apply(terms: Seq[Term]): Term =
    substitutions.foldRight(terms)((sub, terms) => terms.map(sub.apply(_))) match
      case term :: Nil => term
      case Nil         => throw new Exception
      case terms       => Expression(terms.mkString, terms: _*)

  override def compose(other: Unificator): Unificator = other match
    case EmptyUnificator => this
    case un @ SeqUnificator(otherSubstitutions*) =>
      val oldApplied =
        substitutions.map(sub => Substitution(un.apply(sub.t1 :: Nil) -> sub.t2))
      val newUnique =
        otherSubstitutions.filter(sub1 => substitutions.find(sub2 => sub1.t2 == sub2.t2).isEmpty)
      SeqUnificator(oldApplied ++ newUnique: _*)

  override def toString =
    s"{ ${substitutions.map(sub => s"${sub.t1} / ${sub.t2}").mkString(", ")} }"

/** Represent empty unificator with no substitutions. */
case object EmptyUnificator extends Unificator:
  override def apply(terms: Seq[Term]): Term = terms match
    case term :: Nil => term
    case _           => Expression(terms.mkString, terms: _*)

  override def compose(other: Unificator): Unificator = other

  override def toString = "{ }"
