package mw.view

import mw.model.{Squad, Tribe}
import scalafx.scene.control.Tooltip
import scalafx.scene.layout.StackPane
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Circle
import scalafx.scene.text.Text

trait Bubble extends StackPane {
  def centerX: Double
  def centerY: Double
  def tribe: Tribe
  def squadOption: Option[Squad]
  def active: Boolean
  lazy val label: String = squadOption match {
    case Some(squad) => squad.name
    case None => tribe.name
  }
  lazy val color: Color =
    if (!active) LightGrey
    else if (squadOption.isEmpty) Yellow
    else if (tribe.size == 1) LightGreen
    else Cyan
  lazy val shortLabel = if (label.length > 16) label.take(15) + "..." else label
  lazy val center = Point(centerX, centerY)
  val circle = new Circle {
    radius = Bubble.radius
    fill = color
  }
  val text = new Text {
    text = shortLabel
    wrappingWidth = Bubble.wrapping
  }
  if (label.length > 16) {
    val info = new Tooltip(label)
    Tooltip.install(this, info)
  }
  children = circle :: text :: Nil
  layoutX = centerX - Bubble.radius
  layoutY = centerY - Bubble.radius
  override def toString = squadOption match {
    case Some(squad) => s"Bubble(${squad.name})"
    case None => s"Bubble(${tribe.name})"
  }
}
object Bubble {
  val radius = 40
  val wrapping = 50
  def apply(_tribe: Tribe, position: Point, _active: Boolean): Bubble =
    if (_tribe.size == 1) Bubble(_tribe, _tribe.squads.head, position, _active)
    else new Bubble {
      lazy val centerX = position.x
      lazy val centerY = position.y
      lazy val tribe = _tribe
      lazy val squadOption = None
      lazy val active = _active
    }
  def apply(_tribe: Tribe, _squad: Squad, position: Point, _active: Boolean): Bubble = new Bubble {
    lazy val centerX = position.x
    lazy val centerY = position.y
    lazy val tribe = _tribe
    lazy val squadOption = Some(_squad)
    lazy val active = _active
  }
}
