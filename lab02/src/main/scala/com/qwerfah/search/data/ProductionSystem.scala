package com.qwerfah.search.data

/** Rule term - atomic block of rule structure.
  * @param name
  *   Term symbolic name.
  */
final case class Term(name: String):
  require(name.nonEmpty)

/** Single production rule of production system.
  * @param antecedents
  *   Set of antecedents - rule appliance condition.
  * @param consequent
  *   Consequent - rule appliance result.
  */
final case class Rule(
    start: State,
    end: State
):
  require(start.terms.nonEmpty)
  require(end.terms.nonEmpty)

  /** Check if current rule is appliable to the specified deduction state.
    * @param state
    *   Deduction state to check appliance to.
    */
  def isApplicable(state: State) = start.terms.subsetOf(state.terms)

  /** Check if current rule can be unapplied i.e. if its consequent contains in
    * given state.
    * @param state
    *   Deduction state to check unappliance to.
    */
  def isUnapplicable(state: State) = state.terms == end.terms

  /** Apply current rule to the specified state and produce new state as the
    * result of appliance.
    * @param state
    *   Deduction state to apply rule to.
    * @return
    *   New state - the result of rule appliance.
    */
  def apply(state: State) = State(state.terms.diff(start.terms) ++ end.terms)

  /** Apply current rule in reverse order i.e. replace its consequent in given
    * state by its antecedents.
    * @param state
    *   Deduction state to unapply rule to.
    */
  def unapply(state: State) = State(start.terms)

/** Production system with set of production rules.
  * @param rules
  *   Set of production system rules.
  */
final case class ProductionSystem(rules: Set[Rule]):
  require(rules.size > 0)

/** State of the deduction process.
  * @param terms
  *   Set of terms which represent current deduction state.
  */
final case class State(terms: Set[Term], depth: Int = 0, mark: Boolean = false):
  require(terms.size > 0)
  require(depth >= 0)

  def subsetOf(other: State) = terms.subsetOf(other.terms)

  override def equals(other: Any): Boolean = other match
    case state: State => terms.equals(state.terms)
    case _            => false

  override def toString = terms.map(_.name).mkString(", ")

/** Data to organize deduction process. Specify condition and result of solving
  * some task.
  * @param initial
  *   Initial state - set of terms from which deduction process starts.
  * @param target
  *   Final state - set of terms in which deduction process ends.
  */
final case class SearchData(initial: State, target: State)
