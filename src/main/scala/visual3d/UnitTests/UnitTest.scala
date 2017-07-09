package visual3d.UnitTests

import org.junit.Test
import org.junit.Assert.{assertEquals, _}
import visual3d.Gui.Renderer
import visual3d.Model.{Camera, Cuboid, Vector3, World}
import visual3d.Presenter.Projector
/**
  * Created by nikke on 2.3.2017.
  */
class UnitTest {

  @Test def testVertices(): Unit = {
    val screenWidth = 800
    val screenHeight = 600
    val testCube = Cuboid(Vector3(0, 0, 0), Vector3(50, 50, 50))
    val world: World = new World(List(
      testCube
    ))
    val cam = new Camera()
    val projector = Projector(screenWidth, screenHeight, cam, Some(world))
    val edges: List[(Int, Int, Int, Int)] = projector.projectPolygonMesh()


    val vertices = scala.collection.mutable.HashSet[(Int, Int)]()
    for (edge <- edges) {
      val (x1, y1, x2, y2) = edge
      vertices += ((x1, y1))
      vertices += ((x2, y2))
    }
    assert(edges.size < 20, "Testing the amount of edges rendered")
    assert(vertices.size < 14, "Testing the amount of vertices rendered")
  }

  @Test def testSurfaceUnion(): Unit = {
    import visual3d.Model._

    val cuboid = Cuboid(Vector3(0, 0, 0), Vector3(50, 50, 50))
    assertEquals("Testing the amount of surfaces of a cuboid", 6, cuboid.surfaces.length)
  }
}
