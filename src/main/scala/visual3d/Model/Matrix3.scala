package visual3d.Model


/**
  * Class for 3x3 Matrix.
  * @param a First row vector.
  * @param b Second row vector.
  * @param c Third row vector.
  */
case class Matrix3(a: Vector3, b: Vector3, c: Vector3) {
  val matrice: Array[Array[Double]] = Array(a.toArray, b.toArray, c.toArray)


  /**
    * Production of a 3x3 matrix and a 3 dimensional vector.
    * @param v 3 dimensional vector.
    * @return Result of a production as a 3 dimensional vector.
    */
  def *(v: Vector3): Vector3 = {
    val a: Array[Double] = Array(.0, .0, .0)
    val input: Array[Double] = v.array
    val indices = this.matrice.indices

    for (i <- indices) {
      for (j <- indices) {
        a(i) = a(i) + input(j) * this.matrice(i)(j)
      }
    }
    Vector3(a)
  }
}


/**
  * Companion object for Matrix3 class.
  */
object Matrix3 {

  /**
    * Factory method for Matrix3.
    * @param a 2 Dimensional Array of Doubles.
    * @return Returns corresponding Matrix3.
    *         If Array is not in correct dimensions, exception is thrown.
    */
  def apply(a: Array[Array[Double]]): Matrix3 = {
    if (a.length == 3) {
      new Matrix3(
        Vector3(a(0)(0), a(0)(1), a(0)(2)),
        Vector3(a(1)(0), a(1)(1), a(1)(2)),
        Vector3(a(2)(0), a(2)(1), a(2)(2))
      )
    } else {
      throw new IllegalArgumentException(s"Array's length ${a.length} was not 3.")
    }
  }
}
