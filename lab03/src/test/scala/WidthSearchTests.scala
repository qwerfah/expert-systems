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
    val initial = State(Set(Term("place_mountains")))
    val target = State(Set(Term("road_bumpy")))
    val data = SearchData(initial, target)

    val result = search.search(data)
    assert(result.isDefined)

    val graph = result.get
    assertEquals(graph.state, target)
    assert(graph.subgraphs.isDefined)
    assertEquals(graph.subgraphs.get.size, 1)
    assertEquals(graph.subgraphs.get.head.state, initial)

  @Test def widthSearchTest3(): Unit =
    val initial = State(Set(Term("intention_rest"), Term("road_bumpy")))
    val target = State(Set(Term("use_bed")))
    val data = SearchData(initial, target)

    val result = search.search(data)
    assert(result.isEmpty)

  @Test def widthSearchTest4(): Unit =
    val initial = State(Set(Term("intention_rest"), Term("road_bumpy")))
    val target = State(Set(Term("use_jeep")))
    val data = SearchData(initial, target)

    val result = search.search(data)
    assert(result.isDefined)

    val graph = result.get
    assertEquals(graph.state, target)
    assert(graph.subgraphs.isDefined)
    assertEquals(graph.subgraphs.get.size, 1)
    assertEquals(graph.subgraphs.get.head.state, initial)
