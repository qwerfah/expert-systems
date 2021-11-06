import org.junit.{Test, Before}
import org.junit.Assert.*

import com.qwerfah.search.data._
import com.qwerfah.search.xml.Parser
import com.qwerfah.search.fromtarget.DepthSearch

class WidthSearchTests:
  var system: ProductionSystem = _
  var search: DepthSearch = _

  @Before def initialize() =
    system = Parser("system.xml").parse.get
    search = DepthSearch(system)

  @Test def depthSearchTest1(): Unit =
    val initial = State(Set(Term("a")))
    val target = State(Set(Term("b")))
    val data = SearchData(initial, target)

    val result = search.search(data)
    assert(result.isEmpty)

  @Test def depthSearchTest2(): Unit =
    val initial = State(Set(Term("a")))
    val target = State(Set(Term("a")))
    val data = SearchData(initial, target)

    val result = search.search(data)
    assert(result.isDefined)

    val graph = result.get
    assertEquals(graph.state, initial)
    assertEquals(graph.subgraphs, None)

  @Test def depthSearchTest3(): Unit =
    val initial = State(Set(Term("a"), Term("b")))
    val target = State(Set(Term("d")))
    val data = SearchData(initial, target)

    val result = search.search(data)
    assert(result.isDefined)

    val graph = result.get
    assertEquals(graph.state, target)
    assert(graph.subgraphs.isDefined)
    assertEquals(graph.subgraphs.get.size, 1)
    assertEquals(
      graph.subgraphs.get.head.state,
      State(Set(Term("a"), Term("b"), Term("c")))
    )

  @Test def depthSearchTest4(): Unit =
    val initial = State(Set(Term("a"), Term("b"), Term("d")))
    val target = State(Set(Term("d")))
    val data = SearchData(initial, target)

    val result = search.search(data)
    assert(result.isEmpty)

  @Test def depthSearchTest5(): Unit =
    val initial = State(Set(Term("a"), Term("b"), Term("c")))
    val target = State(Set(Term("d")))
    val data = SearchData(initial, target)

    val result = search.search(data)
    assert(result.isDefined)

    val graph = result.get
    assertEquals(graph.state, target)
    assert(graph.subgraphs.isDefined)
    assertEquals(graph.subgraphs.get.size, 1)
    assertEquals(graph.subgraphs.get.head.state, initial)
