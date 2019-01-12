package mw.data

import java.io.{File, FileInputStream}

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

class CSV[T](file: File)(implicit decoder: Decoder[T]) extends Iterator[T] {
  val input = Source.fromInputStream(new FileInputStream(file)).getLines.drop(1)
  def hasNext = input.hasNext
  def next = {
    val line = input.next
    val fields = CSV.parseAll(CSV.record, line) match {
      case CSV.Success(list, _) => list
      case CSV.NoSuccess(msg, _) => throw CsvException(msg)
    }
    decoder(fields) match {
      case (obj, Nil) => obj
      case _ => throw CsvException("Too many fields")
    }
  }
}

object CSV extends RegexParsers {
  def apply[T](file: File)(implicit decoder: Decoder[T]) = new CSV(file)
  def record: Parser[List[String]] = (field ~ ("," ~> field).*).? ^^ {
    case None => Nil
    case Some(head ~ tail) => head :: tail
  }
  def field: Parser[String] = quoted | unquoted
  def quoted: Parser[String] = """("[^\"]*")+""".r ^^ (_.drop(1).dropRight(1).replaceAllLiterally("\"\"", "\""))
  def unquoted: Parser[String] = """[^\"\,]*""".r
}
