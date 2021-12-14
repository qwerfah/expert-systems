package com.qwerfah.unification

@main def main: Unit =
  val un1 = SeqUnificator(
    Substitution(
      Expression("g(x, y)", FunctionalSymbol("g"), Variable("x"), Variable("y")) -> Variable("z")
    )
  )

  val un2 = SeqUnificator(
    Substitution(Constant("A") -> Variable("x")),
    Substitution(Constant("B") -> Variable("y")),
    Substitution(Constant("C") -> Variable("w")),
    Substitution(Constant("D") -> Variable("z"))
  )

  val un = un1 compose un2
  println(un)
