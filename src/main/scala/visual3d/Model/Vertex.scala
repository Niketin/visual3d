package visual3d.Model


/**
  * Vertex class. Vertex is a point, like a location vector.
  * Vertex is mutable. It has many trivial methods that are not explained.
  * @param location
  */
case class Vertex(location: Vector3) {

  def +(v: Vertex): Vertex = Vertex(this.location + v.location)

  def +(v: Vector3): Vertex = Vertex(this.location + v)

  def -(v: Vertex): Vertex = Vertex(this.location - v.location)

  def -(v: Vector3): Vector3 = this.location - v

  def *(d: Double): Vertex = Vertex(this.location * d)

  def /(d: Double): Vertex = Vertex(this.location / d)

  def toArray: Array[Double] = this.location.toArray
  def toList: List[Double] = this.location.toList
}

object Vertex {
  def apply(x: Double, y: Double, z: Double): Vertex = {
    Vertex(Vector3(x, y, z))
  }
}
