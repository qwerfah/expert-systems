package com.qwerfah.search.xml

import scala.xml

import java.io.FileReader
import java.io._

import com.qwerfah.search.data._

class Parser(filename: String):

  val system: xml.Elem =
    xml.XML.loadFile((new File(".")).getAbsolutePath + "/resources/" + filename)

  private def parseAntecedents(ruleNode: xml.Node): Set[Term] =
    (ruleNode \ "antecedents" \ "antecedent").map { node =>
      Term((node \ "@name").text)
    }.toSet

  private def parseConsequent(ruleNode: xml.Node) = Term(
    (ruleNode \ "consequent" \ "@name").text
  )

  private def parseRule(ruleNode: xml.Node) = Rule(
    parseAntecedents(ruleNode),
    parseConsequent(ruleNode)
  )

  def parse = ProductionSystem(
    (system \ "rules" \ "rule").map(parseRule _).toSet
  )
