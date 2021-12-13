package com.qwerfah.unification

trait Unification:
  def unify(t1: Term, t2: Term): Option[Unificator]

object Unification extends Unification:
  override def unify(e1: Term, e2: Term): Option[Unificator] =
    val mayBeTerms = (e1, e2) match
      case (e1: Atom, _) => Some((e1, e2))
      case (_, e2: Atom) => Some((e2, e1))
      case _             => None

    for (e1, e2) <- mayBeTerms do
      return (e1, e2) match
        case (e1, e2) if e1 == e2                  => Some(EmptyUnificator)
        case (e1: Variable, _) if !e2.contains(e1) => Some(SeqUnificator(Substitution(e2 -> e1)))
        case (_, e2: Variable)                     => Some(SeqUnificator(Substitution(e1 -> e2)))
        case _                                     => None

    val (c1, c2) = (e1.asInstanceOf[Expression], e2.asInstanceOf[Expression])
    val (f1, t1) = (c1.terms.head, c1.terms.tail)
    val (f2, t2) = (c2.terms.head, c2.terms.tail)

    unify(f1, f2) match
      case None => None
      case Some(z1) =>
        unify(z1.apply(t1), z1.apply(t2)) match
          case None     => None
          case Some(z2) => Some(z1 compose z2)
