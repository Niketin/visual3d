package visual3d.Model

import scala.collection.mutable


/**
  * Edge class. Edge is a line between two Vertices.
  * @param v1 First Vertex.
  * @param v2 Second Vertex.
  */
case class Edge(v1: Vertex, v2: Vertex) {


  /**
    * Edge to...
    * @return ...Array of Vertices.
    */
  def toArray: Array[Vertex] = Array(v1, v2)


  /**
    * Edge to...
    * @return ...List of Vertices
    */
  def toList: List[Vertex] = List(v1, v2)


  /**
    * Edge to...
    * @return ...Set of Vertices
    */
  def toSet: mutable.Set[Vertex] = mutable.Set(v1, v2)


  /**
    * Swaps Vertices.
    * @return A new Edge.
    */
  def reverse: Edge = Edge(v2, v1)


  /**
    * 3 dimensional vector from first Vertex to second Vertex.
    * @return A new Vector3.
    */
  def getVector: Vector3 = v2.location - v1.location


  /**
    * Checks if this is parallel with another Edge e.
    * @param e Another Edge.
    * @return Boolean value.
    */
  def isParallel(e: Edge): Boolean = this.getVector.isParallel(e.getVector)
}
