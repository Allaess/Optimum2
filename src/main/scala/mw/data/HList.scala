package mw.data

sealed trait HList {
  def ::[H](head: H): H :: this.type = new ::(head, this)
}
case class ::[+H, +T <: HList](head: H, tail: T) extends HList
case object HNil extends HList
