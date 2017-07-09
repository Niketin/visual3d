package visual3d.Presenter

import visual3d.Model._



/**
  * Class for Projector. This calculates all things ready for rendering.
  * @param screenWidth
  * @param screenHeight
  * @param camera
  * @param world
  */
case class Projector(var screenWidth: Int, var screenHeight: Int, var camera: Camera, var world: Option[World] = None) {
  var viewerRelationToDisplay = Vector3(0, 0, -400)
  val viewPlaneOffset: Double = 0.01


  /**
    * Changes resolution.
    * @param x
    * @param y
    */
  def updateSize(x: Int, y: Int): Unit = {
    this.screenWidth = x
    this.screenHeight = y
  }

  /**
    * Returns a list of lines that are going to be projected on the screen.
    * @return
    */
  def projectPolygonMesh(): List[(Int, Int, Int, Int)] = {
    if (world.isDefined){
      for {
        entity <- world.get.entities
        edges = entity.edgesWithoutDuplicates
        edge <- edges
        line: Option[(Int, Int, Int, Int)] = twoDLine(entity, edge.v1, edge.v2)
        if line.isDefined
      } yield line.get
    }
    else Nil
  }


  /**
    * Returns a list of lines that are going to be projected on the screen.
    * Lines are from surfaces that are simplified.
    * @return
    */
  def projectSimplified(): List[(Int, Int, Int, Int)] = {
    if (world.isDefined){
      for {
        entity <- world.get.entities
        surfaces = entity.surfaces
        surface <- surfaces
        edge <- surface.edges
        line: Option[(Int, Int, Int, Int)] = twoDLine(entity, edge.v1, edge.v2)
        if line.isDefined
      } yield line.get
    }
    else Nil
  }


  /**
    * Returns a list of 2D-lines that are converted from Surfaces Edges.
    * @param entity
    * @param surface
    * @return
    */
  private def linesFromSurface(entity: Entity, surface: Surface): List[(Int, Int, Int, Int)] = {
    for {
      edge <- surface.edges
      line: Option[(Int, Int, Int, Int)] = twoDLine(entity, edge.v1, edge.v2)
      if line.isDefined
    } yield line.get
  }


  /**
    * Returns a list that has lists of 2D-lines(for polygon rendering), their angles related to camera, and a distance to camera.
    * @return
    */
  def projectPlanes(): List[(List[(Int, Int, Int, Int)], (Double, Double), Double)] = { // list contains: List of lines, angle for light effect and distance from camera
    if (world.isDefined){
      val lines = for {
        entity <- world.get.entities
        surfaces = entity.triSurfaces
        surface <- surfaces
        lines = linesFromSurface(entity, surface)
        if lines.nonEmpty
        normal: Vector3 = surface.normal
        average: Vector3 = camToVertex(entity, Vertex(surface.averageVertex.location))
        distanceToCamera: Double = average.length
        angleRelatedToViewPlane = camRotation(normal).degreesBetween(Vector3(0, 0, 1))
        angleRelatedToViewingAngle = normal.degreesBetween(average)
        angles = (angleRelatedToViewPlane, angleRelatedToViewingAngle)
        fixedLines = fixCullingLines(lines)
      } yield (fixedLines, angles, distanceToCamera)

      lines
    }
    else {
      Nil
    }
  }

  /**
    * Converts two Vertices to a 2D-line.
    * 2D-line is cut so that it will be only in front of the camera.
    * If it is completely behind camera's view plane, None is returned.
    * @param entity
    * @param vertex1
    * @param vertex2
    * @return
    */
  private def twoDLine(entity: Entity, vertex1: Vertex, vertex2: Vertex): Option[(Int, Int, Int, Int)] = {
    if (vertex1 == vertex2) return None // A line cannot be formed from same two vertices

    val translation1 = camToVertex(entity, vertex1)
    val translation2 = camToVertex(entity, vertex2)
    val rotation1 = camRotation(translation1)
    val rotation2 = camRotation(translation2)

    //Line cutting to view plane
    var line: Option[(Vector3, Vector3)] = None
    (frontOfCamera(rotation1.z), frontOfCamera(rotation2.z)) match {
      case (false, false) => return None  // Line would be behind camera, so None is returned.
      case (false, true) => line = Some(cutLineAtViewPlane(rotation2, rotation1).swap)
      case (true, false) => line = Some(cutLineAtViewPlane(rotation1, rotation2))
      case _ => line = Some(rotation1, rotation2)
    }

    val afterCut1: Vector3 = line.get._1
    val afterCut2: Vector3 = line.get._2

    val x1: Double = viewerRelationToDisplay.z * afterCut1.x / afterCut1.z - viewerRelationToDisplay.x
    val y1: Double = viewerRelationToDisplay.z * afterCut1.y / afterCut1.z - viewerRelationToDisplay.y
    val x2: Double = viewerRelationToDisplay.z * afterCut2.x / afterCut2.z - viewerRelationToDisplay.x
    val y2: Double = viewerRelationToDisplay.z * afterCut2.y / afterCut2.z - viewerRelationToDisplay.y
    val cx1: Int = x1.toInt + screenWidth / 2
    val cy1: Int = y1.toInt + screenHeight / 2
    val cx2: Int = x2.toInt + screenWidth / 2
    val cy2: Int = y2.toInt + screenHeight / 2
    Some(cx1, cy1, cx2, cy2)
  }


  /**
    * Vector3 from camera to vertex. Vertex's original location is related to its entity.
    * @param entity
    * @param vertex
    * @return
    */
  private def camToVertex(entity: Entity, vertex: Vertex): Vector3 = {
    entity.location + vertex.location - camera.location
  }


  /**
    * Calculates Vectors rotation related to Camera's rotation.
    * @param v
    * @return
    */
  private def camRotation(v: Vector3): Vector3 = v.rotateDegY(camera.rotationYaw).rotateDegX(camera.rotationPitch)


  /**
    * v1 stays as it is but v2 is translated to front of the view plane
    * @param v1
    * @param v2
    * @return
    */
  private def cutLineAtViewPlane(v1: Vector3, v2: Vector3): (Vector3, Vector3) = {
    require(v1 != v2)
    val cutDistanceFromPlane: Double =  viewPlaneOffset
    val t: Double = (cutDistanceFromPlane - v1.z) / (v2.z - v1.z)
    val x: Double = v1.x + t*(v2.x - v1.x)
    val y: Double = v1.y + t*(v2.y - v1.y)
    val z: Double  = cutDistanceFromPlane
    (v1, Vector3(x, y, z))
  }


  /**
    * Returns true if z-coomponent is big enough. Same as in front of the camera.
    * @param z
    * @return
    */
  private def frontOfCamera(z: Double): Boolean = z >= viewPlaneOffset


  /**
    * Returns true if 2D-point is inside screen.
    * @param x
    * @param y
    * @return
    */
  private def insideBounds(x: Int, y: Int): Boolean = x < screenWidth && x >=0 && y < screenHeight && y >=0


  /**
    * Some polygon lines might be not continuous so this fixes the problem.
    * @param lines
    * @return
    */
  private def fixCullingLines(lines: List[(Int, Int, Int, Int)]): List[(Int, Int, Int, Int)] = {
    require(lines.length >= 2)
    val fixedLines =
      for ((x, y) <- lines.zip( lines.last +: lines.dropRight(1) )) yield {
        val (x1, x2,_,_) = x
        val (_,_,y3, y4) = y

        val result = if (x1 == y3 && x2 == y4) List(x)
                     else List((y3, y4, x1, x2), x)
        result
      }
    fixedLines.flatten
  }
}
