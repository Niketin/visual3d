package visual3d.Gui

import java.awt.{BasicStroke, Color, RenderingHints}

import visual3d.Presenter._
import visual3d.Utils.clamp

import scala.swing.Graphics2D
import scala.math._
import java.awt.geom.{GeneralPath, Path2D}


/**
  * Class for Renderer. Needed for rendering.
  * @param projector Gives all the Entity information to Renderer.
  */
case class Renderer(projector: Projector) {

  private def screenWidth: Int = projector.screenWidth
  private def screenHeight: Int = projector.screenHeight

  /**
    * Renders everything
    * @param g
    * @param wireFrame Renders wireFrame.
    * @param simplified Renders wireFrame simplified.
    * @param planes Renders Polygons/planes.
    * @param lightMode Renders colors with different settings.
    */
  def render(g: Graphics2D,
             wireFrame: Boolean,
             simplified: Boolean,
             planes: Boolean,
             lightMode: Boolean
            ): Unit = {

    g.setColor(Color.WHITE)
    g.fillRect(0,0,screenWidth, screenHeight)


    def renderLines(simplified: Boolean = true): Unit = {
      val edges: List[(Int, Int, Int, Int)] = if (simplified) projector.projectSimplified() else projector.projectPolygonMesh()

      val vertices = scala.collection.mutable.HashSet[(Int, Int)]()
      for (edge <- edges) {
        val (x1, y1, x2, y2) = edge
        g.drawLine(x1, y1, x2, y2)
        vertices += ((x1, y1))
        vertices += ((x2, y2))
      }
    }

    def renderPlanes(): Unit = {
      val surfaces: List[(List[(Int, Int, Int, Int)], (Double, Double), Double)] = projector.projectPlanes().sortBy(-_._3)
      for (surfaceData <- surfaces if surfaceData._1.nonEmpty) {
        val surface = surfaceData._1
        val (angleRelatedToViewPlane, angleRelatedToViewingAngle): (Double, Double) = surfaceData._2

        // Prepare polygon
        val polygonalChain = new GeneralPath(Path2D.WIND_NON_ZERO, 10)
        polygonalChain.moveTo(surface.head._1, surface.head._2)
        for (line <- surface) {
          polygonalChain.lineTo(line._3, line._4)
        }

        // Ignore surfaces that cannot possibly be in sight because of their current rotation.
        if (angleRelatedToViewingAngle > 90) {

          // Draw polyogn according to lighting settings.
          if (lightMode) {
            val ratio = clamp[Double]((angleRelatedToViewPlane - 90) / 90.0 * 1.5 + 0.2, 0.4, 1)
            g.setColor(new Color(
              (253 * ratio).toInt,
              (102 * ratio).toInt,
              0,
              255
            ))
            g.fill(polygonalChain)
          }
          else {
              val ratio = clamp[Double]((angleRelatedToViewingAngle - 90) / 90.0 + 0.5, 0, 1)
              g.setColor(new Color(
                (253 * ratio).toInt,
                (102 * ratio).toInt,
                0,
                255
              ))
              g.fill(polygonalChain)
          }
        }
      }
    }

    if (planes) renderPlanes()

    if (wireFrame) {
      g.setColor(Color.black)
      g.setStroke(new BasicStroke (1))
      renderLines(simplified)
    }

    // Info Area.
    {
      val infoX = 5
      val infoY = 5
      val (fps, ups) = (App.currentFPS, App.currentUPS)

      //Transparent background.
      g.setColor(new Color(0, 0, 0, 70))
      g.fillRect(infoX - 2, infoY - 2, 140, 195)

      // Antialiasing makes text smoother without wasting much performance.
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

      g.setColor(new Color(255, 0, 0))
      val location = projector.camera.location.toArray.map(x => "%.1f".format(x))
      val infoText =
        StringContext.treatEscapes(
          s"""
           |Camera
           |  x: ${location(0)}
           |  y: ${location(1)}
           |  z: ${location(2)}
           |  Yaw: ${"%.1f".format(projector.camera.rotationYaw)}
           |  Pitch: ${"%.1f".format(projector.camera.rotationPitch)}
           |
           |Horizontal FOV: ${"%.1f".format(toDegrees(2 * atan(screenWidth / 2 / abs(projector.viewerRelationToDisplay.z))))}
           |Vertical FOV: ${"%.1f".format(toDegrees(2 * atan(screenHeight/ 2 / abs(projector.viewerRelationToDisplay.z))))}
           |
           |FPS: $fps
           |UPS: $ups
          """.stripMargin
        )

      var y = infoY
      for (line <- infoText.split('\n')) {
        g.drawString(line, infoX, y)
        y += g.getFontMetrics.getHeight
      }

    }
  }
}
