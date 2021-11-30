package com.qwerfah.search.fromdata

import scala.collection.immutable.Queue

import com.qwerfah.search.data._

/** Breadth search implementation for deduction process in a and-or graph. And-or
  * graph is a formal representation of the solution space of some task.
  * Deduction process organizes using [[ProductionSystem]] instance data. And-or
  * graph produced during deduction process is subgraph of solution space of
  * initial task and represents the process of solving specified with
  * [[SearchData]] task.
  * @param system
  *   Production system instance.
  */
class BreadthSearch(private val system: ProductionSystem):
  private val queue: Queue[State] = Queue()

  /** Get set of successors in and-or graph for specified deduction state.
    * @param state
    *   State to get successors for.
    */
  def getSuccessors(state: State) = system.rules.collect {
    case r if r.isApplicable(state) => r.apply(state)
  }

  private def recursiveSearch(
      state: State,
      f: (State, Set[State]) => (Queue[State], Set[State])
  ): StateGraph =
    def recurse(
        q: Queue[State],
        visited: Set[State],
        graph: StateGraph
    ): StateGraph =
      if (q.isEmpty) then graph
      else
        val (node, tail) = q.dequeue
        val (childs, v) = f(node, visited + node)
        recurse(tail ++ childs, v, graph.addAll(childs, node))

    val (childs, visited) = f(state, Set(state))
    recurse(
      Queue.empty ++ childs,
      visited,
      StateGraph(state).addAll(childs, state)
    )

  /** Search the decision of specified task using breadth search in and-or
    * graph.
    * @param data
    *   Task condition and result of solving.
    */
  def search(data: SearchData): Option[StateGraph] =
    val graph = recursiveSearch(
      data.initial,
      (state, visited) => {
        val successors = getSuccessors(state)
        val notVisited = successors.filterNot(visited.contains _)
        (Queue(notVisited.toSeq: _*), visited ++ notVisited)
      }
    )
    if graph.contains(data.target) then Some(graph)
    else None

/*
    queue.clear()
    queue.enqueue(data.initial)
    var graph = StateGraph(data.initial)

    for {
      state <- queue.reverse if queue.nonEmpty
      successors = getSuccessors(state)
      notVisited = successors.filterNot(visited.contains _)
     } {
      queue.enqueueAll(notVisited)
      visited.addAll(notVisited)
      notVisited.map(graph.add(_, state))
     }

    while queue.nonEmpty do
      val state = queue.dequeue()
      if (data.target.subsetOf(state)) return Some(graph)

      visited.add(state)

      val successors = getSuccessors(state)
      val notVisited = successors.filterNot(visited.contains _)

      queue.enqueueAll(notVisited)
      visited.addAll(notVisited)
      notVisited.map(graph.add(_, state))

    None
 */
