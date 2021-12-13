package com.qwerfah.unification

trait Term:
    val name: String
    
    def contains(other: Term): Boolean

trait Atom extends Term:
    override def contains(other: Term): Boolean = this == other

case class PredicateSymbol(override val name: String) extends Atom

case class FunctionalSymbol(override val name: String) extends Atom

case class Constant(override val name: String) extends Atom

case class Variable(override val name: String) extends Atom

case class Expression(override val name: String, terms: Term*) extends Term:
    override def contains(other: Term): Boolean = terms.contains(other)