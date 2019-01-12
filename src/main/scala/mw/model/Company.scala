package mw.model

import java.io.{BufferedWriter, File, FileOutputStream, OutputStreamWriter}

import mw.data._
import mw.model

import scala.collection.mutable
import scala.util.Try

trait Company {
  outer =>
  def tribes: List[Tribe]
  def tribe(name: String): Option[Tribe] = tribes.find(_.name == name)
  def tribe(squad: Squad) = tribes.find(_.contains(squad))
  def size = tribes.size
  def score = {
    val weights = for {
      tribe1 <- this
      tribe2 <- this if tribe1.name < tribe2.name
    } yield tribe1 <-> tribe2
    weights.sum
  }
  def merge(tribe1: Tribe, tribe2: Tribe) = Company {
    (tribe1 ++ tribe2) :: outer.tribes.filter { t => t != tribe1 && t != tribe2 }
  }
  def move(squad: Squad, to: Tribe) = Company {
    tribe(squad) match {
      case Some(from) =>
        if (from.size > 1) (from - squad) :: (to + squad) :: outer.tribes.filter { t => t != from && t != to }
        else from :: (to + squad) :: outer.tribes.filter({ t => t != from && t != to })
      case None => (to + squad) :: outer.tribes.filter(_ != to)
    }
  }
  def severe(squad: Squad) = move(squad, Tribe.empty)
  def explode = Company {
    for {
      tribe <- this
      squad <- tribe
    } yield Tribe(squad)
  }
  def next = {
    val links = for {
      tribe1 <- this
      tribe2 <- this if tribe1.name < tribe2.name
    } yield (tribe1, tribe1 <-> tribe2, tribe2)
    val maxWeight = Try(links.map(_._2).max).getOrElse(0)
    links.find(_._2 == maxWeight)
  }
  def optimize: Company = {
    val nextCompany = for ((tribe1, _, tribe2) <- next) yield merge(tribe1, tribe2)
    nextCompany match {
      case Some(company) => company.optimize
      case None => this
    }
  }
  def save(file: File) = {
    def format(field: String) =
      if (field.contains("\"") || field.contains(","))
        s""""${field.replaceAllLiterally("\"", "\"\"")}""""
      else field
    val output = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))
    output.write("Tribe 1,Squad 1,Weight,Squad 2,Tribe 2")
    output.newLine()
    for {
      tribe1 <- this
      tribe2 <- this
      squad1 <- tribe1
      squad2 <- tribe2
      weight = squad1 <-> squad2
      if weight > 0
    } {
      output.write(s"${format(tribe1.name)},${format(squad1.name)},$weight,${format(squad2.name)},${format(tribe2.name)}")
      output.newLine()
    }
    output.close()
  }
  def map[S](f: Tribe => S) = tribes.map(f)
  def flatMap[S](f: Tribe => Iterable[S]) = tribes.flatMap(f)
  def withFilter(p: Tribe => Boolean) = tribes.filter(p)
  def foreach(action: Tribe => Any) = tribes.foreach(action)
  override def toString = s"Company($size tribes)"
}
object Company {
  val empty: Company = new model.Company.Immutable(Nil)
  def apply(tribes: List[Tribe]): Company = new Immutable(tribes)
  def apply(file: File): Company = {
    val company = new Company.Mutable
    for (tribeName1 :: squadName1 :: weight :: squadName2 :: tribeName2 :: HNil <-
           CSV[String :: String :: Int :: String :: String :: HNil](file)) {
      val tribe1 = company.tribe(tribeName1).getOrElse(new Tribe.Mutable(tribeName1))
      val tribe2 = company.tribe(tribeName2).getOrElse(new Tribe.Mutable(tribeName2))
      company += tribe1
      company += tribe2
      val squad1 = tribe1.squad(squadName1).getOrElse(new Squad.Mutable(squadName1))
      val squad2 = tribe2.squad(squadName2).getOrElse(new Squad.Mutable(squadName2))
      tribe1 += squad1
      tribe2 += squad2
      squad1 += squad2 -> weight
      squad2 += squad1 -> weight
    }
    company
  }
  class Mutable extends Company {
    private val tribesMap = mutable.Map.empty[String, Tribe.Mutable]
    def tribes = tribesMap.values.toList
    override def tribe(name: String) = tribesMap.get(name)
    def +=(tribe: Tribe.Mutable): Unit = tribesMap += tribe.name -> tribe
  }
  class Immutable(val tribes: List[Tribe]) extends Company
}
