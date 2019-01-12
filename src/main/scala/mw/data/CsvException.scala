package mw.data

class CsvException(msg: String) extends Exception(msg)
object CsvException {
  def apply(msg: String) = new CsvException(msg)
}