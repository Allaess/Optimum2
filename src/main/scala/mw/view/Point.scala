package mw.view

import scala.util.Random

case class Point(x: Double, y: Double) {
  def +(that: Point) = Point(this.x + that.x, this.y + that.y)
  def -(that: Point) = Point(this.x - that.x, this.y - that.y)
  def *(d: Double) = Point(x * d, y * d)
  def /(d: Double) = Point(x / d, y / d)
  def unary_- = Point(-x, -y)
  def size = math.sqrt(x * x + y * y)
  def max(that: Point) = Point(math.max(this.x, that.x), math.max(this.y, that.y))
}
object Point {
  val origin = Point(0, 0)
  def random(max: Point) = Point(Random.nextDouble * max.x, Random.nextDouble * max.y)
}
