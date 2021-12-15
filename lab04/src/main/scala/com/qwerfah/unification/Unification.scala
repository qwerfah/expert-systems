package com.qwerfah.unification

trait Unification:
  def unify(t1: Term, t2: Term): Option[Unificator]

object Unification extends Unification:
  override def unify(e1: Term, e2: Term): Option[Unificator] =
    val mayBeTerms = (e1, e2) match
      case (_: Atom, _) => Some((e1, e2))
      case (_, _: Atom) => Some((e2, e1))
      case _            => None

    val mayBeUnificator = mayBeTerms.flatMap {
        case (e1, e2) if e1 == e2 => Some(EmptyUnificator)
        case (e1: Variable, e2) if !e2.contains(e1) => Some(SeqUnificator(Substitution(e2 -> e1)))
        case (e1, e2: Variable) => Some(SeqUnificator(Substitution(e1 -> e2)))
        case _ => None
    }

    (mayBeTerms, mayBeUnificator) match
      case (Some(_), unificator @ Some(_)) => unificator
      case (Some(_), None) => None
      case _ =>
        val (c1, c2) = (e1.asInstanceOf[Expression], e2.asInstanceOf[Expression])
        val (f1, t1) = (c1.terms.head, c1.terms.tail)
        val (f2, t2) = (c2.terms.head, c2.terms.tail)

        unify(f1, f2) match
          case Some(z1) =>
            val a1 = z1.apply(t1)
            val a2 = z1.apply(t2)
            unify(a1, a2) match
              case Some(z2) => Some(z1 compose z2)
              case _        => None
          case _ => None
