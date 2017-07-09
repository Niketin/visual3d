package visual3d.Model


/**
  * Surface class. It is like a simple polygon.
  * Surface cannot have any holes.
  * @param edges
  */
case class Surface(edges: List[Edge]) {
  require(edges.length >= 3)
  def this(face: Face) {
    this(face.toList)
  }

  lazy val averageVertex: Vertex = getAverageVertex


  /**
    * Calculates average Vertex from Surface's Vertices.
    * @return
    */
  def getAverageVertex: Vertex = {
    val vertices: List[Vertex] = edges.map(_.v1)
    val vertexCount: Int = vertices.length
    val average = vertices.reduce((a, b) => a + b ) / vertexCount
    average
  }


  /**
    * Surface's normal that points to the outer side of this Surface.
    * @return
    */
  def normal: Vector3 = {
    val e = edges.zip(edges.tail :+ edges.head).find(x => !x._1.isParallel(x._2)).get
    e._1
      .getVector
      .crossProduct(e._2.getVector)
      .normalize
  }


  /**
    * Checks if this Surface contains a given Edge.
    * @param edge
    * @return Boolean value.
    */
  def contains(edge: Edge): Boolean = {
    val firstFind = this.edges.find(x => x == edge || x == edge.reverse)
    if (firstFind.isDefined) {
      true
    }
    else {
      val secondFind = this.edges.find(_ == edge.reverse)
      if (secondFind.isDefined) true
      else false
    }
  }


  /**
    * Finds all commond Edges of this Surface and a Face.
    * @param face
    * @return List of common Edges.
    */
  def commonEdges(face: Face): List[Edge] = {
    for {
      edge
      <- face.toList
      isCommonEdge: Boolean = this.contains(edge)
      if isCommonEdge
    } yield edge
  }


  /**
    * Checks if Face is adjacent to this Surface.
    * @param face
    * @return Boolean Value.
    */
  def isAdjacent(face: Face): Boolean = {
    val common = commonEdges(face)
    this.normal == face.normal && common.nonEmpty && common.size < 3
  }


  /**
    * Unions with List of Faces.
    * @param faces
    * @return Returns a new Surface and all the remaining, non fitting Faces, in a list.
    */
  def unionWithFaces(faces: List[Face]): (Surface, List[Face]) = {
    var newSurface = this
    var remainingFaces = faces

    var running = true
    while (running) {
      val union = newSurface.unionWithAdjacentFaces(remainingFaces)
      newSurface = union._1
      if (remainingFaces == union._2) running = false
      remainingFaces = union._2
    }
    (newSurface, remainingFaces)
  }


  /**
    * Unions with List of adjacent Faces.
    * @param faces
    * @return Returns a new Surface and all the remaining, non fitting Faces, in a list.
    */
  def unionWithAdjacentFaces(faces: List[Face]): (Surface, List[Face]) = {
    val adjacents = faces.filter(x => this.isAdjacent(x))
    var newSurface = this
    for (face <- adjacents) {
      newSurface = newSurface.union(face)
    }

    val remainingFaces = faces diff adjacents
    (newSurface, remainingFaces)
  }

  /**
    * Union with adjacent face.
    * @param face Requires face to be adjacent.
    * @return A new unioned Surface.
    */
  def unionWithAdjacent(face: Face): Surface = {
    require(this.isAdjacent(face))
    this.union(face)
  }


  /**
    * Unions this and a Face.
    * Assumes they are possible to union.
    * @param face
    * @return A new Unioned Surface.
    */
  private def union(face: Face): Surface = {
    val edges = this.commonEdges(face)
    val sEdges = this.edges
    edges.length match {
      case 1 if edges.head == face.toList(1) =>
        Surface(sEdges.patch(sEdges.indexOf(edges.head.reverse), face.toList.diff(edges ++ edges.map(_.reverse)).reverse, 1))
      case 1 =>
        Surface(sEdges.patch(sEdges.indexOf(edges.head.reverse), face.toList.diff(edges ++ edges.map(_.reverse)), 1))
      case 2
        if (sEdges.indexOf(edges.head.reverse) == 0 &&
          sEdges.indexOf(edges.last.reverse) == sEdges.length - 1) ||
          (sEdges.indexOf(edges.last.reverse) == 0 &&
          sEdges.indexOf(edges.head.reverse) == sEdges.length - 1) =>
            Surface(
              face.toList
                .tail
                .dropRight(1)
                .++(face.toList.diff(edges ++ edges.map(_.reverse))))

      case 2 =>
        val earlierIndex = scala.math.min(sEdges.indexOf(edges.head.reverse), sEdges.indexOf(edges.last.reverse))
        Surface(sEdges.patch(
          earlierIndex,
          face.toList.diff(edges ++ edges.map(_.reverse)),
          2))
      case _ => Surface(List())
    }
  }
}

