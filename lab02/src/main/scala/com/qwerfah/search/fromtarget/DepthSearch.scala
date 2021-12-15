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
object DepthSearch:
  private val stack: Stack[State] = Stack()

  /** Get set of successors in and-or graph for specified deduction state.
    * @param state
    *   State to get successors for.
    */
  def getSuccessors(state: State) = system.rules.collect {
    case r if r.isUnapplicable(state) => r.unapply(state)
  }

  private def markdown(state: State, rules: Set[Rule]): Set[Rule] =
    def recur(state: State): Boolean =
      rules.filter(r => r.end == state).exists(r => r.start.terms.forall(t => markdown(State(Set[Term](t)), rules)))

  /** Search from target to data in state space.
    * @param data
    *   Task condition and result of solving.
    */
  def search(
      data: SearchData,
      system: ProductionSystem,
      boarder: Int = 100
  ): Option[Set[Rule]] =
    val opened = Stack[State](data.target)
    var closed = Set[State]()
    var graph = Set[Rule]()

    while !opened.isEmpty do
      var n = opened.pop()
      closed += n
      if n.depth > boarder then n = n.copy(mark = true)
      else
        val rules = system.rules.filter(rule => rule.isUnapplicable(n))
        opened.pushAll(rules.map(_.unapply(n)))
        graph ++= rules
        if rules.isEmpty then n = n.copy(mark = true)
      if n.mark then graph = markdown(n, graph)





    None
