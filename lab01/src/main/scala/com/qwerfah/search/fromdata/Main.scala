package com.qwerfah.search.fromdata

import scala.util.{Success, Failure}

import com.qwerfah.search.xml.Parser
import com.qwerfah.search.dot.ToDotFileSerializer
import com.qwerfah.search.data.SearchData
import com.qwerfah.search.data.{Term, State}
import com.qwerfah.search.data.ProductionSystem

def loop(system: ProductionSystem) =
  // while true do
  var initialTerms = Array[Term]()
  var targetTerms = Array[Term]()

  while initialTerms.isEmpty do
    print("Space separated list of terms for initial state: ")
    val initial = "intention_rest place_mountains" // scala.io.StdIn.readLine()
    initialTerms = initial.split(" ").filter(!_.isBlank).map(Term(_))
    if initialTerms.isEmpty then println("Empty list, try again")

  while targetTerms.isEmpty do
    print("Space separated list of terms for final state: ")
    val target = "use_jeep" // scala.io.StdIn.readLine()
    targetTerms = target.split(" ").filter(!_.isBlank).map(Term(_))
    if targetTerms.isEmpty then println("Empty list, try again")

  val searchData = SearchData(State(initialTerms), State(targetTerms))

  println(searchData)

  BreadthSearch.search(searchData, system) match
    case Some(rules) =>
      println("\nTarget state is reachable\n")
      ToDotFileSerializer.serialize("graph.dot", rules, searchData) match
        case Success(_) => println("\nDecision graph saved\n")
        case Failure(e) => println(s"\nUnable to save graph: ${e.getMessage}\n")
    case None => println("\nTarget state is unreachable\n")

@main def hello: Unit =
  val path = "./system.xml"
  print(s"\nProduction system xml representation file path [$path]: ")
  val customPath = "" // scala.io.StdIn.readLine()

  Parser(if customPath.isBlank then path else customPath).parse match
    case Success(system) => loop(system)
    case Failure(e)      => println(s"\nUnable to parse xml: ${e.getMessage}\n")
