package com.qwerfah.search.fromdata

import scala.collection.mutable.Queue

import com.qwerfah.search.data._

class Search(private val base: KnowledgeBase):
  private val queue: Queue[Term] = Queue()

  private def isAllowed(target: Seq[Term]) = target.toSet.subsetOf(queue.toSet)

  private def deduct(term: Term): Seq[Term] =
    def isApplicable = (rule: Rule) =>
      (rule.antecedents.length == 1 || rule.ruleType == RuleType.Or) && rule.antecedents
        .contains(term) || (rule.ruleType == RuleType.And && )
    base.rules filter isApplicable map { rule => rule.consequent }

  def produce(data: SearchData): Boolean =
    queue.clear
    queue.enqueueAll(data.data)

    while queue.nonEmpty && !isAllowed(data.target) do
      val term = queue.dequeue
      queue.enqueueAll(deduct(term))

    true
