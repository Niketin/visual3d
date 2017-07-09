package visual3d.Model

import scala.math.{cos, sin, sqrt, toRadians}


/**
  * Class for 3-dimensional vectors.
  * Non-trivial methods are explained.
  * @param x
  * @param y
  * @param z
  */
case class Vector3(x: Double, y: Double, z: Double) {
  var array: Array[Double] = Array(x, y, z)

  def +(v: Vector3): Vector3 = {
    Vector3(this.x + v.x, this.y + v.y, this.z + v.z)
  }

  def unary_- : Vector3 = Vector3(-this.x, -this.y, -this.z)

  def -(v: Vector3): Vector3 = {
    Vector3(this.x - v.x, this.y - v.y, this.z - v.z)
  }

  def *(d: Double): Vector3 = Vector3(this.x * d, this.y * d, this.z * d)

  def *(v: Vector3): Vector3 = {
    Vector3(this.x * v.x, this.y * v.y, this.z * v.z)
  }

  def /(s: Double): Vector3 = {
    Vector3(this.x / s, this.y / s, this.z / s)
  }

  def dotProduct(v: Vector3): Double = this.x * v.x + this.y * v.y + this.z * v.z

  def crossProduct(v: Vector3): Vector3 = {
    Vector3(
      this.y*v.z - this.z*v.y,
      this.z*v.x - this.x*v.z,
      this.x*v.y - this.y*v.x
    )
  }

  def isSameDir(v: Vector3): Boolean = this.normalize == v.normalize
  def isOppositeDir(v: Vector3): Boolean = this.normalize == -v.normalize
  def isParallel(v: Vector3): Boolean = this.isOppositeDir(v) || this.isSameDir(v)

  def opposite: Vector3 = this * Vector3(-1, -1, -1)

  def degreesBetween(v: Vector3): Double = scala.math.toDegrees(radiansBetween(v))
  def radiansBetween(v: Vector3): Double = scala.math.acos(this.dotProduct(v)/(this.length*v.length))

  def length: Double = sqrt(this.dotProduct(this))

  def normalize: Vector3 = {
    val len = this.length
    this / len
  }

  def distance(o: Vector3): Double = {
    val diff = o - this
    diff.length
  }


  /**
    * Rotates Vector3 with a rotation-matrix. Rotation is around X-axis.
    * @param rad Radians.
    * @return
    */
  def rotateX(rad: Double): Vector3 = {
    val yzRotationMatrix = Matrix3(Array(
      Array(1, 0, 0),
      Array(0, cos(rad), -sin(rad)),
      Array(0, sin(rad), cos(rad))
    ))
    yzRotationMatrix * this
  }


  /**
    * Rotates Vector3 with a rotation-matrix. Rotation is around Y-axis.
    * @param rad Radians.
    * @return
    */
  def rotateY(rad: Double): Vector3 = {
    val xzRotationMatrix = Matrix3(Array(
      Array(cos(rad), 0, sin(rad)),
      Array(0, 1, 0),
      Array(-sin(rad), 0, cos(rad))
    ))
    xzRotationMatrix * this
  }


  /**
    * Rotates Vector3 with a rotation-matrix. Rotation is around Z-axis.
    * @param rad Radians.
    * @return
    */
  def rotateZ(rad: Double): Vector3 = {
    val xyRotationMatrix = Matrix3(Array(
      Array(cos(rad), -sin(rad), 0),
      Array(sin(rad), cos(rad), 0),
      Array(0, 0, 1)
    ))
    xyRotationMatrix * this
  }


  // These are same as above rotation methods except they take argument as degrees.
  def rotateDegX(deg: Double): Vector3 = rotateX(toRadians(deg))
  def rotateDegY(deg: Double): Vector3 = rotateY(toRadians(deg))
  def rotateDegZ(deg: Double): Vector3 = rotateZ(toRadians(deg))

  def scale(s1: Double, s2: Double, s3: Double): Vector3 = {
    this * Vector3(s1, s2, s3)
  }

  def toInt: (Int, Int, Int) = (this.x.toInt, this.y.toInt, this.z.toInt)

  def toArray: Array[Double] = Array[Double](x, y, z)
  def toList: List[Double] = List[Double](x, y, z)

  override def toString: String = s"x: ${x.toFloat},y: ${y.toFloat},z: ${z.toFloat}"
}

object Vector3 {
  def apply(a: Array[Double]): Vector3 = {
    if (a.length == 3) {
      Vector3(a(0), a(1), a(2))
    } else {
      throw new IllegalArgumentException(s"Array's length ${a.length} was not 3.")
    }
  }
}
