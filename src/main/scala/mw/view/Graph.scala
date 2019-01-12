package mw.view

import mw.model.{Company, Squad, Tribe}

import scala.collection.mutable

class Graph(val company: Company, size: Point = Point(1920, 1080)) {
  outer =>
  protected val tribePositions = mutable.Map.empty[Tribe, Point]
  protected val squadPositions = mutable.Map.empty[Squad, Point]
  protected val tribeRadii = mutable.Map.empty[Tribe, Double]
  def position(tribe: Tribe): Point = tribePositions.get(tribe) match {
    case Some(point) => point
    case None =>
      val r = radius(tribe) + 100
      val border = Point(r, r)
      val point = Point.random(size) + border
      tribePositions += tribe -> point
      point
  }
  def position(squad: Squad): Point = squadPositions.get(squad) match {
    case Some(point) => point
    case None => company.tribe(squad) match {
      case Some(tribe) =>
        val index = tribe.indexOf(squad)
        val point =
          if (tribe.size == 1) position(tribe)
          else position(tribe) + relativePosition(index)
        squadPositions += squad -> point
        point
      case None =>
        val border = Point(100, 100)
        Point.random(size) + border
    }
  }
  def to(company: Company, positions: Map[Tribe, Point] = Map.empty) = new Graph(company, size) {
    override protected val tribePositions = outer.tribePositions.clone
    for {
      tribe <- company if tribe.size == 1
      squad <- tribe
    } tribePositions += tribe -> outer.position(squad)
    for (pair <- positions)
      tribePositions += pair
    layout()
  }
  private def relativePosition(index: Int) = {
    def helper(indexInLayer: Int, layerSize: Int, layerRadius: Double): Point =
      if (indexInLayer < layerSize) {
        val angle = math.toRadians(360.0 / layerSize * indexInLayer)
        Point(-math.cos(angle) * layerRadius, -math.sin(angle) * layerRadius)
      } else helper(indexInLayer - layerSize, layerSize + 6, layerRadius + 100.0)
    helper(index, 6, 100.0)
  }
  private def radius(tribe: Tribe) = tribeRadii.get(tribe) match {
    case Some(result) => result
    case None =>
      def helper(countInLayer: Int, layerSize: Int, layerRadius: Double): Double =
        if (countInLayer <= layerSize) layerRadius
        else helper(countInLayer - layerSize, layerSize + 6, layerRadius + 100.0)
      val tribeSize = tribe.size
      val result =
        if (tribeSize <= 1) 0.0
        else helper(tribeSize, 6, 100.0)
      tribeRadii += tribe -> result
      result
  }
  protected def layout(): Unit = {
    val corrections = mutable.Map.empty[Tribe, Point]
    var moved = false
    for {
      tribe1 <- company
      tribe2 <- company if tribe1.name < tribe2.name
    } {
      val oldPosition1 = position(tribe1)
      val oldPosition2 = position(tribe2)
      val gapVector = oldPosition2 - oldPosition1
      val oldDistance = gapVector.size
      val minDistance = radius(tribe1) + radius(tribe2) + 100.0
      if (oldDistance < minDistance) {
        val distanceCorrection = (minDistance - oldDistance) / 2.0
        if (distanceCorrection > 1.0) {
          moved = true
          val correction1 = gapVector * (-distanceCorrection / oldDistance)
          val correction2 = gapVector * (distanceCorrection / oldDistance)
          corrections.get(tribe1) match {
            case Some(correction) => corrections += tribe1 -> (correction + correction1)
            case None => corrections += tribe1 -> correction1
          }
          corrections.get(tribe2) match {
            case Some(correction) => corrections += tribe2 -> (correction + correction2)
            case None => corrections += tribe2 -> correction2
          }
        }
      }
    }
    if (moved) {
      for ((tribe, correction) <- corrections) {
        val oldPosition = position(tribe)
        val r = radius(tribe) + 40
        val newPosition = (oldPosition + correction) max Point(r, r)
        tribePositions += tribe -> newPosition
      }
      squadPositions.clear()
      layout()
    }
  }
  override def toString = s"Graph(${company.size} tribes)"
}
object Graph {
  val empty = new Graph(Company.empty)
}
