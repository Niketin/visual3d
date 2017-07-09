package visual3d.Model

import scala.collection.mutable

/**
  * Trait of an Entity.
  */
trait Entity {

  var location: Vector3

  lazy val faces: List[Face] = getFaces
  lazy val edges: List[Edge] = getEdges
  lazy val edgesWithoutDuplicates: List[Edge] = getEdgesWithoutDuplicates
  lazy val vertices: List[Vertex] = getVertices
  lazy val surfaces: List[Surface] = getSurfaces
  lazy val triSurfaces: List[Surface] = getTriSurfaces

  /**
    * This is implemented in inheriting classes.
    * @return List of Faces.
    */
  def getFaces: List[Face]


  /**
    * Gets all the Edges from Entity's Faces.
    * @return List of Edges.
    */
  def getEdges: List[Edge] = {
    val edges: mutable.ListBuffer[Edge] = mutable.ListBuffer()
    faces.foreach(face => edges ++= face.toList )
    edges.toList
  }


  /**
    * Same as getEdges but without duplicates.
    * @return List of Edges without duplicates.
    */
  def getEdgesWithoutDuplicates: List[Edge] = {
    var vertices = List[Edge]()
    for (edge <- edges)
      if (!vertices.contains(edge) && !vertices.contains(edge.reverse)) {
        vertices = vertices :+ edge
    }
    vertices
  }


  /**
    * Gets all Vertices from Edges. Default is without duplicates.
    * @return List of Vertices.
    */
  def getVertices: List[Vertex] = {
    val vertices: mutable.Set[Vertex] = mutable.Set()
    getEdges.foreach(edge => vertices ++= edge.toSet)
    vertices.toList
  }


  /**
    * Basicly maps all Faces to Surfaces.
    * @return List of Surfaces.
    */
  private def getTriSurfaces: List[Surface] = faces.map(face => Surface(face.toList))


  /**
    * Gets all Entity's Surfaces. Surfaces are combined with adjacent surfaces if possible.
    * @return List of Surfaces.
    */
  private def getSurfaces: List[Surface] = {
    val faces: List[Face] = this.faces

    val commonNormals: List[List[Face]] =
      faces.groupBy(_.normal)
        .toList
        .map(_._2)

    val surfaces: List[List[Surface]] =
      for(faces <- commonNormals) yield {
        var finalSurfaces = List[Surface]()
        var remainingFaces = faces
        while (remainingFaces.nonEmpty) {
          val face = remainingFaces.head
          remainingFaces = remainingFaces.diff(Seq(face))
          val (surface, rFaces) = new Surface(face).unionWithFaces(remainingFaces)
          finalSurfaces = finalSurfaces :+ surface
          remainingFaces = rFaces
        }
        finalSurfaces
      }
    surfaces.flatten
  }
}
