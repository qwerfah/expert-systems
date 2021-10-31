package com.qwerfah.search.fromdata

import com.qwerfah.search.xml.Parser

@main def hello: Unit =
  val parser = Parser("knowledgebase.xml")
  println(parser.parse())

def msg = "I was compiled by Scala 3. :)"
