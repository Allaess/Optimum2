package mw.model

import scala.collection.Map
import scala.collection.mutable

trait Squad {
  def name: String
  def weights: Map[Squad, Int]
  def <->(that: Squad) = weights.getOrElse(that, 0)
  def <->(that: Tribe) = that <-> this
  override def equals(that: Any) = that match {
    case that: Squad => this.name == that.name
    case _ => false
  }
  override def hashCode = name.hashCode
  override def toString = s"Squad($name)"
}
object Squad {
  class Mutable(val name: String) extends Squad {
    val weights = mutable.Map.empty[Squad, Int]
    def +=(pair: (Squad, Int)): Unit = weights += pair
  }
}
