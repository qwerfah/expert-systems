import com.qwerfah.unification._

import org.junit.Test
import org.junit.Assert.*

class SubstitutionTests:
  @Test def test1(): Unit =
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
    assert(un.isInstanceOf[SeqUnificator])

    val seqUn = un.asInstanceOf[SeqUnificator]
    val exprSub = Substitution(Expression("gAB", FunctionalSymbol("g"), Constant("A"), Constant("B")) -> Variable("z"))
    val aSub = Substitution(Constant("A") -> Variable("x"))
    val bSub = Substitution(Constant("B") -> Variable("y"))
    val cSub = Substitution(Constant("C") -> Variable("w"))
    assertEquals(seqUn.substitutions, Seq(exprSub, aSub, bSub, cSub))
