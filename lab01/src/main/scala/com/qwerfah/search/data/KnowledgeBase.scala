package com.qwerfah.search.data

enum RuleType:
  case And, Or

sealed abstract class Term(name: String)

final case class Antecedent(name: String) extends Term(name):
  require(name.nonEmpty)

final case class Consequent(name: String) extends Term(name):
  require(name.nonEmpty)

final case class Rule(
    antecedents: Seq[Antecedent],
    consequent: Consequent,
    ruleType: RuleType
):
  require(antecedents.length > 0)

final case class KnowledgeBase(rules: Seq[Rule]):
  require(rules.length > 0)

final case class SearchData(data: Seq[Term], target: Seq[Term])
