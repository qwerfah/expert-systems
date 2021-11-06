import org.junit.Test
import org.junit.Assert.*

import com.qwerfah.search.xml.Parser
import com.qwerfah.search.data.{Rule, Term}

class ParserTests:
  @Test def parserTest1(): Unit =
    val parser = Parser("some.xml")
    val result = parser.parse
    assert(result.isFailure)

  @Test def parserTest2(): Unit =
    val result = Parser("test1.xml").parse
    assert(result.isFailure)

  @Test def parserTest3(): Unit =
    val result = Parser("test2.xml").parse
    assert(result.isFailure)

  @Test def parserTest4(): Unit =
    val result = Parser("test3.xml").parse
    assert(result.isSuccess)

    val system = result.get
    val expected = Set[Rule](
      Rule(Set(Term("a"), Term("b")), Term("c")),
      Rule(Set(Term("d")), Term("e"))
    )

    assertEquals(system.rules, expected)
