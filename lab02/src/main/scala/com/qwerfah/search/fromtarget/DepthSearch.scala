package com.qwerfah.search.fromtarget

import scala.collection.mutable.Stack

import com.qwerfah.search.data._

/** Depth search implementation for deduction process in a and-or graph from
  * target to data. And-or graph is a formal representation of the solution
  * space of some task. Deduction process organizes using [[ProductionSystem]]
  * instance data. And-or graph produced during deduction process is subgraph of
  * solution space of initial task and represents the process of solving
  * specified with [[SearchData]] task.
  * @param system
  *   Production system instance.
  */
class DepthSearch(private val system: ProductionSystem):
  private val stack: Stack[State] = Stack()

  /** Get set of successors in and-or graph for specified deduction state.
    * @param state
    *   State to get successors for.
    */
  def getSuccessors(state: State) = system.rules.collect {
    case r if r.isUnapplicable(state) => r.unapply(state)
  }

  /** Search from target to data in state space.
    * @param data
    *   Task condition and result of solving.
    */
  def search(data: SearchData): Option[StateGraph] =
    stack.clear()
    stack.push(data.target)
    var visited = collection.mutable.Set[State]()
    var graph = StateGraph(data.target)

    while stack.nonEmpty do
      val state = stack.pop
      if (data.initial.subsetOf(state)) return Some(graph)

      visited.add(state)

      val successors = getSuccessors(state)
      val notVisited = successors.filterNot(visited.contains _)

      stack.pushAll(notVisited)
      visited.addAll(notVisited)
      notVisited.map(graph.add(_, state))

    None
