import com.qwerfah.unification._

import org.junit.Test
import org.junit.Assert.*

class UnificationTests:
  @Test def test1(): Unit =
    val t1 = Expression("P(x)", FunctionalSymbol("P"), Variable("x"))
    val t2 = Expression("P(A)", FunctionalSymbol("P"), Constant("A"))
    val mayBeUnificator = Unification.unify(t1, t2)
    val unificator = SeqUnificator(Substitution(Constant("A") -> Variable("x")))

    assertTrue(mayBeUnificator.isDefined)
    assertEquals(mayBeUnificator.get, unificator)

  @Test def test2(): Unit =
    val t1 = Expression(
      "P(f(x), y, g(y))",
      PredicateSymbol("P"),
      FunctionalSymbol("f"),
      Variable("x"),
      Variable("y"),
      FunctionalSymbol("g"),
      Variable("y")
    )
    val t2 = Expression(
      "P(f(x), z, g(x))",
      PredicateSymbol("P"),
      FunctionalSymbol("f"),
      Variable("x"),
      Variable("z"),
      FunctionalSymbol("g"),
      Variable("x")
    )

    val mayBeUnificator = Unification.unify(t1, t2)
    val unificator = SeqUnificator(
      Substitution(Variable("x") -> Variable("y")),
      Substitution(Variable("x") -> Variable("z"))
    )

    assertTrue(mayBeUnificator.isDefined)
    assertEquals(mayBeUnificator.get, unificator)
