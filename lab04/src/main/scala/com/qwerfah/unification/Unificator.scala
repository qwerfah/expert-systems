package com.qwerfah.unification

case class Substitution(raw: (Term, Variable)):
  def t1: Term = raw._1
  def t2: Variable = raw._2

  override def equals(obj: Any): Boolean = obj match
    case Substitution(raw) => this.raw == raw
    case _                 => false

  def apply(term: Term): Term = term match
    case Variable(_) if term == raw._2 => raw._1
    case Expression(_, terms*) =>
      val applied = terms.map(apply)
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
    substitutions.foldRight(terms)((sub, terms) => terms.map(sub.apply)) match
      case Seq(terms*) =>
        if terms.length == 1 then terms.head else Expression(terms.mkString, terms: _*)
      case _ => throw new Exception("empty expression")

  override def compose(other: Unificator): Unificator = other match
    case EmptyUnificator => this
    case un @ SeqUnificator(otherSubstitutions*) =>
      val oldApplied =
        substitutions.map(sub => Substitution(un.apply(sub.t1 :: Nil) -> sub.t2))
      val newUnique =
        otherSubstitutions.filter(sub1 => !substitutions.exists(sub2 => sub1.t2 == sub2.t2))
      SeqUnificator(oldApplied ++ newUnique: _*)

  override def toString =
    s"{ ${substitutions.map(sub => s"${sub.t1}/${sub.t2}").mkString(", ")} }"

/** Represent empty unificator with no substitutions. */
case object EmptyUnificator extends Unificator:
  override def apply(terms: Seq[Term]): Term = terms match
    case Seq(terms*) =>
      if terms.length == 1 then terms.head else Expression(terms.mkString, terms: _*)
    case _ => throw new Exception("empty expression")

  override def compose(other: Unificator): Unificator = other

  override def toString = "{ }"
