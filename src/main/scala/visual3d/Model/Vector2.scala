package visual3d.Model

import scala.math.sqrt


/**
  * Class for 2-dimensional vectors.
  * Contains trivial methods.
  * @param x
  * @param y
  */
case class Vector2(x: Double, y: Double) {

  def +(o: Vector2): Vector2 = {
    Vector2(this.x + o.x, this.y + o.y)
  }

  def -(o: Vector2): Vector2 = {
    Vector2(this.x - o.x, this.y - o.y)
  }

  def /(s: Double): Vector2 = {
    Vector2(this.x / s, this.y / s)
  }

  def length: Double = {
    sqrt(this.x * this.x + this.y * this.y)
  }

  def normal: Vector2 = {
    val len = this.length
    this / len
  }

  def distance(o: Vector2): Double = {
    val diff = o - this
    diff.length
  }

  def toInt: (Int, Int) = (this.x.toInt, this.y.toInt)



}
