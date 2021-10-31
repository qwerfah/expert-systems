package com.qwerfah.search.xml

import scala.xml

import java.io.FileReader
import java.io._

import com.qwerfah.search.data._

class Parser(filename: String):

  val knowledgebase: xml.Elem =
    xml.XML.loadFile((new File(".")).getAbsolutePath + "/resources/" + filename)

  private def parseAntecedents(ruleNode: xml.Node): Seq[Antecedent] =
    (ruleNode \ "antecedents" \ "antecedent").map { node =>
      Antecedent((node \ "@name").text)
    }

  private def parseConsequent(ruleNode: xml.Node): Consequent = Consequent(
    (ruleNode \ "consequent" \ "@name").text
  )

  private def parseRule(ruleNode: xml.Node): Rule = Rule(
    parseAntecedents(ruleNode),
    parseConsequent(ruleNode),
    RuleType.valueOf((ruleNode \ "@type").text)
  )

  def parse(): KnowledgeBase = KnowledgeBase(
    (knowledgebase \ "rules" \ "rule") map { parseRule(_) }
  )
