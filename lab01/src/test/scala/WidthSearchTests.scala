import org.junit.{Test, Before}
import org.junit.Assert.*

import com.qwerfah.search.data._
import com.qwerfah.search.xml.Parser
import com.qwerfah.search.fromdata.WidthSearch

class WidthSearchTests:
  var system: ProductionSystem = _
  var search: WidthSearch = _

  @Before def initialize() =
    system = Parser("system.xml").parse.get
    search = WidthSearch(system)

  @Test def widthSearchTest1(): Unit =
    val initial = State(Set(Term("a")))
    val target = State(Set(Term("b")))
    val data = SearchData(initial, target)

    val result = search.search(data)
    assert(result.isEmpty)

  @Test def widthSearchTest2(): Unit =
    val initial = State(Set(Term("a")))
    val target = State(Set(Term("a")))
    val data = SearchData(initial, target)

    val result = search.search(data)
    assert(result.isDefined)

    val graph = result.get
    assertEquals(graph.state, initial)
    assertEquals(graph.subgraphs, None)

  @Test def widthSearchTest3(): Unit =
    val initial = State(Set(Term("a"), Term("b")))
    val target = State(Set(Term("d")))
    val data = SearchData(initial, target)

    val result = search.search(data)
    assert(result.isEmpty)

  @Test def widthSearchTest4(): Unit =
    val initial = State(Set(Term("a"), Term("b"), Term("c")))
    val target = State(Set(Term("d")))
    val data = SearchData(initial, target)

    val result = search.search(data)
    assert(result.isDefined)

    val graph = result.get
    assertEquals(graph.state, initial)
    assert(graph.subgraphs.isDefined)
    assertEquals(graph.subgraphs.get.size, 1)
    assertEquals(graph.subgraphs.get.head.state, target)
