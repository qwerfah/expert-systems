package com.qwerfah.search.fromdata

import scala.collection.immutable.Queue

import com.qwerfah.search.data._

/** Width search implementation for and-or graph. And-or graph represented by
  * [[KnowledgeBase]] instance.
  * @param base
  *   And-or graph representation.
  */
class WidthSearch(private val base: KnowledgeBase):
  private var queue: Queue[Term] = Queue()

  /** Perform single step of search algorithm.
    * @return
    *   Collection pair: collection of new terms to enqueue and collection of
    *   terms to dequeue (head and some other terms in case of and-rule match).
    */
  private def step: (Seq[Term], Set[Term]) =
    val term = queue.head

    /** Check if or-rule matches with current queue head. */
    def isOrRuleMatches(rule: Rule) =
      (rule.antecedents.length == 1 || rule.ruleType == RuleType.Or) && rule.antecedents
        .contains(term)

    /** Check if and-rule matches with current queue content (not only head if
      * rule contains more that one antecedent).
      */
    def isAndRuleMatches(rule: Rule) =
      rule.ruleType == RuleType.And && rule.antecedents.toSet.subsetOf(
        queue.toSet
      )

    /** Check if arbitrary rule is applicable to current queue state. */
    def isApplicable = (rule: Rule) =>
      isOrRuleMatches(rule) || isAndRuleMatches(rule)

    val applicable = base.rules filter isApplicable

    (
      applicable map { rule => rule.consequent },
      applicable.map(_.antecedents).flatten.toSet[Term]
    )

  def search(data: SearchData): Boolean =
    def isAllowed(target: Seq[Term]) = target.toSet.subsetOf(queue.toSet)

    queue = Queue().enqueueAll(data.data)

    while queue.nonEmpty && !isAllowed(data.target) do
      val result = step
      queue = queue.dropWhile(result._2.contains(_)).enqueueAll(result._1)

    true
