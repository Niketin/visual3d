package visual3d.Model

import scala.collection.mutable

/**
  * Face class. Face is a collection of three Edges. Face is constructed counterclockwise.
  * @param e1 First Edge.
  * @param e2 Second Edge.
  * @param e3 Third Edge.
  */
case class Face(e1: Edge, e2: Edge, e3: Edge) {

  /**
    * Face to Array of Edges
    * @return
    */
  def toArray: Array[Edge] = Array(e1, e2, e3)


  /**
    * Face to List of Edges
    * @return
    */
  def toList: List[Edge] = List(e1, e2, e3)


  /**
    * Face to Set of Edges
    * @return
    */
  def toSet: mutable.Set[Edge] = mutable.Set(e1, e2, e3)


  /**
    * Normal of the visible side of the Face.
    * @return
    */
  def normal: Vector3 = {
    e1.getVector
      .crossProduct(e3.reverse.getVector)
      .normalize
  }
}


/**
  * Companion object of the Face class
  */
object Face {

  /**
    * Factory method for Face. Vertices must be in CCW.
    * @param v1
    * @param v2
    * @param v3
    * @return
    */
  def apply(v1: Vertex, v2: Vertex, v3: Vertex): Face = {
    Face(
      Edge(v1, v2),
      Edge(v2, v3),
      Edge(v3, v1)
    )
  }
}
