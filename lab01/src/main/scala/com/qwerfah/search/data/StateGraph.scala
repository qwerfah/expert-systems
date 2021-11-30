package com.qwerfah.search.data

final case class StateGraph(
    val state: State,
    val subgraphs: Option[Seq[StateGraph]] = None
):

  def addAll(childs: Seq[State], parent: State): StateGraph =
    def addToSubgraphs = subgraphs match
      case Some(graphs) => Some(graphs.map(_.addAll(childs, parent)))
      case _ => None

    def addSubgraph = subgraphs match 
      case Some(graphs) => Some(graphs ++ childs.map(StateGraph(_)))
      case _ => Some(childs.map(StateGraph(_)))

    if state == parent then StateGraph(state, addSubgraph)
    else StateGraph(state, addToSubgraphs)

  def contains(item: State): Boolean = 
    def containsInSubgraphs = subgraphs match
      case Some(graphs) => graphs.find(_.contains(item)).isDefined
      case _ => false

    if item.subsetOf(state) then true else containsInSubgraphs
