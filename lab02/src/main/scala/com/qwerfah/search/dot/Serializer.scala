package com.qwerfah.search.dot

import scala.util.{Try, Success, Failure}

import java.io.{File, FileWriter}

import com.qwerfah.search.data.State
import com.qwerfah.search.data.StateGraph

class Serializer(graph: StateGraph):
  private def createWriter(filename: String): Try[FileWriter] =
    Try { new FileWriter(new File(filename)) }

  private def serialize(writer: FileWriter, target: State): Try[Unit] =
    def serialize(graph: StateGraph): Unit =
      graph.subgraphs match
        case Some(_) => writer.write(s"\"[${graph.state}]\"")
        case None if target == graph.state =>
          writer.write(s"\"[${graph.state}]\"[color=green]\n")
        case _ => writer.write(s"\"[${graph.state}]\"[color=red]\n")

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

  def serialize(filename: String, target: State): Try[Unit] =
    for
      writer <- createWriter(filename)
      result <- serialize(writer, target)
    yield result
