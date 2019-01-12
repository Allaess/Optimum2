package mw.data

trait Decoder[+T] {
  def apply(fields: List[String]): (T, List[String])
}
object Decoder {
  implicit val string: Decoder[String] = { list =>
    if (list.isEmpty) throw CsvException("Not enough fields")
    else (list.head, list.tail)
  }
  implicit val int: Decoder[Int] = { list =>
    if (list.isEmpty) throw CsvException("Not enough fields")
    else (list.head.toInt, list.tail)
  }
  implicit val hNil: Decoder[HNil] = { list => (HNil, list) }
  implicit def hList[H, T <: HList](implicit headDecoder: Decoder[H], tailDecoder: Decoder[T]): Decoder[H :: T] = { list =>
    val (headValue, headRest) = headDecoder(list)
    val (tailValue, tailRest) = tailDecoder(headRest)
    (headValue :: tailValue, tailRest)
  }
}
