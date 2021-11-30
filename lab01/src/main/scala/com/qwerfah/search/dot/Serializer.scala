package com.qwerfah.search.dot

import scala.util.{Try, Success, Failure}

import java.io.File

import com.qwerfah.search.data.StateGraph
import java.io.FileWriter
import com.qwerfah.search.data.SearchData

class Serializer(graph: StateGraph):
  private def createWriter(filename: String): Try[FileWriter] =
    Try { new FileWriter(new File(filename)) }

  private def serialize(writer: FileWriter, data: SearchData): Try[Unit] =
    def serialize(graph: StateGraph): Unit =
      val fillcolor =
        if graph.state == data.initial then "\"green\""
        else if graph.state == data.target then "\"red\""
        else "\"white\""
      writer.write(s"\"${graph.state}\" [style=filled fillcolor=$fillcolor]\n")
      graph.subgraphs.getOrElse(Seq[StateGraph]()) foreach { g =>
        writer.write(s"\"${graph.state}\" -> \"${g.state}\"\n")
        serialize(g)
      }

    Try {
      writer.write("digraph G {\n")
      serialize(graph)
      writer.write("}")
      writer.close()
    }

  def serialize(filename: String, data: SearchData): Try[Unit] =
    for
      writer <- createWriter(filename)
      result <- serialize(writer, data)
    yield result
