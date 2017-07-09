package visual3d.Model

/**
  * Cuboid entity.
  * @param location Location in world's coordinate system.
  * @param size 3 dimensional scale of the cuboid.
  */
case class Cuboid(var location: Vector3, size: Vector3) extends Entity {

  val (sx, sy, sz) = (size.x/2, size.y/2, size.z/2)

  val v1 = Vertex(-sx, -sy, -sz)
  val v2 = Vertex(-sx, -sy,  sz)
  val v3 = Vertex( sx, -sy, -sz)
  val v4 = Vertex( sx, -sy,  sz)
  val v5 = Vertex(-sx,  sy, -sz)
  val v6 = Vertex(-sx,  sy,  sz)
  val v7 = Vertex( sx,  sy, -sz)
  val v8 = Vertex( sx,  sy,  sz)

  val faceList: List[Face] = {
    List[Face](
      Face(v1, v3, v2), //Bottom side
      Face(v4, v2, v3),

      Face(v2, v6, v1), //Right side
      Face(v5, v1, v6),

      Face(v4, v8, v2),//Rear side
      Face(v6, v2, v8),

      Face(v3, v7, v4),//Left side
      Face(v8, v4, v7),

      Face(v1, v5, v3),//Front side
      Face(v7, v3, v5),

      Face(v5, v6, v7),//Top side
      Face(v8, v7, v6)
    )
  }


  def getFaces: List[Face] = faceList

}
