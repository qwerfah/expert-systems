package com.qwerfah.search.dot

import scala.util.{Try, Success, Failure}

import java.io.{File, FileWriter}

import com.qwerfah.search.data.{SearchData, Rule, Term}

trait Serializer:
  def serialize(filename: String, rules: Seq[Rule], data: SearchData): Try[Unit]

object ToDotFileSerializer extends Serializer:
  private def createWriter(filename: String): Try[FileWriter] =
    Try { new FileWriter(new File(filename)) }

  private def serialize(writer: FileWriter, rules: Seq[Rule], data: SearchData): Try[Unit] =
    def defineFillcolor(term: Term): String =
      if data.initial.terms.contains(term) then "\"green\""
      else if data.target.terms.contains(term) then "\"red\""
      else "\"white\""

    Try {
      writer.write("digraph G {\n")

      if data.target isSubstateOf data.initial then
        writer.write(s"\"${data.target}\" [style=filled fillcolor=red]\n")
      else for rule <- rules do writer.write(s"$rule\n")

      writer.write("}")
      writer.close()
    }

  override def serialize(filename: String, rules: Seq[Rule], data: SearchData): Try[Unit] =
    for
      writer <- createWriter(filename)
      result <- serialize(writer, rules, data)
    yield result
