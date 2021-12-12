package com.qwerfah.search.fromdata

import com.qwerfah.search.data._

object BreadthSearch:

  /** Search the decision of specified task using breadth search in and-or graph.
    * @param data
    *   Task condition and result of solving.
    */
  def search(data: SearchData, system: ProductionSystem): Option[Seq[Rule]] =
    def mutateState(state: State, applied: Seq[Rule]): (State, Seq[Rule]) =
      val applicable = system.rules.filter { rule =>
        !applied.contains(rule) && rule.isApplicable(state)
      }
      (applicable.foldRight(state) { (rule, state) => rule.apply(state) }, applied ++ applicable)

    def recur(state: State, applied: Seq[Rule] = Nil): (State, Seq[Rule]) =
      if data.target.terms.forall(state.terms.contains(_)) then
        (State(state.terms, target = true), applied)
      else
        mutateState(state, applied) match
          case (newState @ State(_, true, _), rules) => recur(State(newState.terms), rules)
          case newState                              => newState

    recur(State(data.initial.terms)) match
      case (state @ State(_, _, true), rules) => Some(rules)
      case _                                  => None
