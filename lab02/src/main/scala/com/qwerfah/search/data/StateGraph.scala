package com.qwerfah.search.data

final case class StateGraph(
    val state: State,
    var subgraphs: Option[Seq[StateGraph]] = None
):
  def addSubgraph(subgraph: StateGraph): Option[Seq[StateGraph]] =
    Some(subgraphs.getOrElse(Seq[StateGraph]()) :+ subgraph)

  def add(child: State, parent: State): Unit =
    if state == parent then subgraphs = addSubgraph(StateGraph(child))
    else subgraphs.getOrElse(Seq[StateGraph]()).map(_.add(child, parent))
