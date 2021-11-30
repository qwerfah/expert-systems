package com.qwerfah.unification

trait Term {
    val name: String
}

case class PredicateSymbol() extends Term

case class FunctionalSymbol() extends Term

case class Constant() extends Term

case class Variable() extends Term

case class 