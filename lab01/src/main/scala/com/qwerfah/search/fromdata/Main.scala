package com.qwerfah.search.fromdata

import com.qwerfah.search.xml.Parser
import com.qwerfah.search.data.{Term, State}
import com.qwerfah.search.data.SearchData

@main def hello: Unit =
  val path = "./system.xml"
  print(s"\nProduction system xml representation file path [$path]: ")
  val customPath = scala.io.StdIn.readLine()

  val parser = Parser(if customPath.isBlank then path else customPath)
  val system = parser.parse
  println(s"\nProduction system: $system\n")

  val search = WidthSearch(system)
  val searchData =
    SearchData(
      State(Set(Term("a"), Term("b"), Term("c"), Term("d"))),
      State(Set(Term("h")))
    )

  println(search.search(searchData))

/*
  while true do
    var initialTerms = Array[Term]()
    var targetTerms = Array[Term]()


    while initialTerms.isEmpty do
      print("Space separated list of terms for initial state: ")
      val initial = scala.io.StdIn.readLine()
      initialTerms = initial.split(" ").filter(!_.isBlank).map(Term(_))
      if initialTerms.isEmpty then println("Empty list, try again")

    while targetTerms.isEmpty do
      print("Space separated list of terms for final state: ")
      val target = scala.io.StdIn.readLine()
      targetTerms = target.split(" ").filter(!_.isBlank).map(Term(_))
      if targetTerms.isEmpty then println("Empty list, try again")
 */
