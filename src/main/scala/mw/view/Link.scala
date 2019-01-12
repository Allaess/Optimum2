package mw.view

import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Line

class Link(bubble1: Bubble, bubble2: Bubble, weight: Double, status: Link.Status) extends Line {
  startX = bubble1.centerX
  startY = bubble1.centerY
  endX = bubble2.centerX
  endY = bubble2.centerY
  strokeWidth = math.max(weight, 1)
  stroke = status.color
}
object Link {
  def apply(bubble1: Bubble, bubble2: Bubble, weight: Double, status: Link.Status): Link =
    new Link(bubble1, bubble2, weight, status)
  sealed abstract class Status(val color: Color)
  case object IntraTribe extends Status(Black)
  case object Inactive extends Status(LightGray)
  case object Selected extends Status(Red)
  case object Alternative extends Status(Cyan)
}
