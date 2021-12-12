package com.qwerfah.search.xml

import scala.xml
import scala.util.{Try, Success, Failure}

import java.io.FileReader
import java.io._

import cats.implicits._

import com.qwerfah.search.data._

class Parser(filename: String):
  private def openXmlFile: Try[xml.Elem] =
    Try {
      xml.XML.loadFile(
        (new File(".")).getAbsolutePath + "/resources/" + filename
      )
    }

  private def parseAntecedents(ruleNode: xml.Node): Try[Seq[Term]] =
    Try {
      (ruleNode \ "antecedents" \ "antecedent").map { node =>
        Term((node \ "@name").text)
      }
    }

  private def parseConsequent(ruleNode: xml.Node): Try[Term] =
    Try { Term((ruleNode \ "consequent" \ "@name").text) }

  private def parseRule(ruleNode: xml.Node): Try[Rule] = for
    antecedents <- parseAntecedents(ruleNode)
    consequent <- parseConsequent(ruleNode)
  yield Rule(antecedents, consequent)

  def parse: Try[ProductionSystem] = for
    system <- openXmlFile
    rules <- (system \ "rules" \ "rule").map(parseRule _).sequence
  yield ProductionSystem(rules)
