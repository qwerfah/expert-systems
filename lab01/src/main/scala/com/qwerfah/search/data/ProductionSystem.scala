package com.qwerfah.search.data

/** Rule term - atomic block of rule structure.
  * @param name
  *   Term symbolic name.
  */
final case class Term(name: String) extends AnyVal:
  override def toString = s"\"$name\""

/** Single production rule of production system.
  * @param antecedents
  *   Set of antecedents - rule appliance condition.
  * @param consequent
  *   Consequent - rule appliance result.
  */
final case class Rule(
    antecedents: Seq[Term],
    consequent: Term
):
  require(antecedents.size > 0)

  /** Check if current rule is appliable to the specified deduction state.
    * @param state
    *   Deduction state to check appliance to.
    */
  def isApplicable(state: State) = antecedents.forall(state.terms.contains(_))

  /** Apply current rule to the specified state and produce new state as the result of appliance.
    * @param state
    *   Deduction state to apply rule to.
    * @return
    *   New state - the result of rule appliance.
    */
  def apply(state: State) = State(state.terms :+ consequent, true)

  override def toString: String = s"{ ${antecedents
    .mkString(" ")} } -> $consequent ${if antecedents.length > 1 then "[color=red]" else ""}"

/** Production system with set of production rules.
  * @param rules
  *   Set of production system rules.
  */
final case class ProductionSystem(rules: Seq[Rule]):
  require(rules.size > 0)

/** State of the deduction process.
  * @param terms
  *   Set of terms which represent current deduction state.
  */
final case class State(terms: Seq[Term], mutated: Boolean = false, target: Boolean = false):
  require(terms.size > 0)

  def isSubstateOf(other: State): Boolean = terms.forall(other.terms.contains(_))

  override def equals(other: Any): Boolean = other match
    case state: State => terms.equals(state.terms)
    case _            => false

  override def toString = s"(${terms.map(_.name).mkString(", ")})"

/** Data to organize deduction process. Specify condition and result of solving some task.
  * @param initial
  *   Initial state - set of terms from which deduction process starts.
  * @param target
  *   Final state - set of terms in which deduction process ends.
  */
final case class SearchData(initial: State, target: State)
