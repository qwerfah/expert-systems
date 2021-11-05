package com.qwerfah.search.dot

import scala.util.{Try, Success, Failure}

import java.io.File

import com.qwerfah.search.data.StateGraph
import java.io.FileWriter

class Serializer(graph: StateGraph):
  private def createWriter(filename: String): Try[FileWriter] =
    Try { new FileWriter(new File(filename)) }

  private def serialize(writer: FileWriter): Try[Unit] =
    def serialize(graph: StateGraph): Unit =
      writer.write(s"\"[${graph.state}]\"")
      graph.subgraphs.getOrElse(Seq[StateGraph]()) foreach { g =>
        writer.write(s"\"[${graph.state}]\" -> \"[${g.state}]\"\n")
        serialize(g)
      }

    Try {
      writer.write("digraph G {\n")
      serialize(graph)
      writer.write("}")
      writer.close()
    }
    
  def serialize(filename: String): Try[Unit] =
    for
      writer <- createWriter(filename)
      result <- serialize(writer)
    yield result
