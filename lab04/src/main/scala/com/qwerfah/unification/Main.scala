package com.qwerfah.unification

@main def main(): Unit =
  /** val t1 = Expression( "P(f(x), y, g(y))", PredicateSymbol("P"), FunctionalSymbol("f"),
    * Variable("x"), Variable("y"), FunctionalSymbol("g"), Variable("y") ) val t2 = Expression(
    * "P(f(x), z, g(x))", PredicateSymbol("P"), FunctionalSymbol("f"), Variable("x"), Variable("z"),
    * FunctionalSymbol("g"), Variable("x") )
    */

  val t1 = Expression(
    "P(f(x, g(A, y)), g(A, y))",
    PredicateSymbol("P"),
    Expression(
      "f(x, g(A, y))",
      FunctionalSymbol("f"),
      Variable("x"),
      Expression("g(A, y)", FunctionalSymbol("g"), Constant("A"), Variable("y"))
    ),
    Expression("g(A, y)", FunctionalSymbol("g"), Constant("A"), Variable("y"))
  )
  val t2 = Expression(
    "P(f(x, z), z)",
    PredicateSymbol("P"),
    Expression("f(x, z)", FunctionalSymbol("f"), Variable("x"), Variable("z")),
    Variable("z")
  )

  Unification.unify(t1, t2) match
    case Some(unificator) => println(s"Terms can be unified, common unificator is $unificator")
    case _                => println("Terms can't be unified")
