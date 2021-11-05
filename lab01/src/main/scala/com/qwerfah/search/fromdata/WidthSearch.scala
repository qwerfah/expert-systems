package com.qwerfah.search.fromdata

import scala.collection.mutable.Queue

import com.qwerfah.search.data._

/** Width search implementation for deduction process in a and-or graph. And-or
  * graph is a formal representation of the solution space of some task.
  * Deduction process organizes using [[ProductionSystem]] instance data. And-or
  * graph produced during deduction process is subgraph of solution space of
  * initial task and represents the process of solving specified with
  * [[SearchData]] task.
  * @param system
  *   Production system instance.
  */
class WidthSearch(private val system: ProductionSystem):
  private val queue: Queue[State] = Queue()

  /** Get set of successors in and-or graph for specified deduction state.
    * @param state
    *   State to get successors for.
    */
  def getSuccessors(state: State) = system.rules.collect {
    case r if r.isApplicable(state) => r.apply(state)
  }

  /** Search the decision of specified task using with search in and-or graph.
    * @param data
    *   Task condition and result of solving.
    */
  def search(data: SearchData): Boolean =
    queue.clear()
    queue.enqueue(data.initial)

    while queue.nonEmpty do
      val state = queue.dequeue()
      if (state.subsetOf(data.target)) return true
      val successors = getSuccessors(state)
      queue.enqueueAll(successors)

    false