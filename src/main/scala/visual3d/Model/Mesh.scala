package visual3d.Model


/**
  * Class for a Mesh (renderable object).
  * @param location Presents the location of the Mesh in world's coordinate system.
  * @param faceList List of Mesh's faces
  */
case class Mesh(var location: Vector3, faceList: List[Face]) extends Entity {

  /**
    * Implements Entitys method 'getFaces'.
    * @return l Returns List of Faces
    */
  def getFaces: List[Face] = faceList
}