package mw.model

import mw.model

import scala.collection.mutable

trait Tribe {
  outer =>
  def name: String
  def squads: List[Squad]
  def squad(name: String): Option[Squad] = squads.find(_.name == name)
  def size = squads.size
  def contains(squad: Squad) = squads.contains(squad)
  def indexOf(squad: Squad) = squads.indexOf(squad)
  def <->(that: Tribe) = {
    val weights = for {
      thisSquad <- this
      thatSquad <- that
    } yield thisSquad <-> thatSquad
    weights.sum
  }
  def <->(that: Squad) = {
    val weights = for (squad <- this if squad != that) yield squad <-> that
    weights.sum
  }
  def ++(that: Tribe) = new Tribe {
    val name = outer.name
    val squads = outer.squads ++ that.squads
  }
  def +(_squad: Squad) = new Tribe {
    val name = outer.name
    val squads = _squad :: outer.squads
  }
  def -(_squad: Squad) = new Tribe {
    val name = outer.name
    val squads = outer.squads.filter(_ != _squad)
  }
  def map[S](f: Squad => S) = squads.map(f)
  def flatMap[S](f: Squad => Iterable[S]) = squads.flatMap(f)
  def withFilter(p: Squad => Boolean) = new model.Tribe.Immutable(name, squads.filter(p))
  def foreach(action: Squad => Any) = squads.foreach(action)
  override def equals(that: Any) = that match {
    case that: Tribe => this.name == that.name
    case _ => false
  }
  override def hashCode = name.hashCode
  override def toString = s"Tribe($name)"
}
object Tribe {
  private var count = 0
  private def newName = {
    count += 1
    s"Tribe $count"
  }
  def empty: Tribe = new Immutable(newName, Nil)
  def apply(squad: Squad): Tribe = new Immutable(newName, squad :: Nil)
  class Mutable(val name: String) extends Tribe {
    private val squadsMap = mutable.Map.empty[String, Squad.Mutable]
    def squads = squadsMap.values.toList
    override def squad(name: String) = squadsMap.get(name)
    def +=(squad: Squad.Mutable): Unit = squadsMap += squad.name -> squad
  }
  class Immutable(val name: String, val squads: List[Squad]) extends Tribe
}
