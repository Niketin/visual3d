package visual3d.Presenter

import visual3d.Gui.App._
import visual3d.Model.{Camera, Vector3}
import scala.swing.event._
import scala.collection.mutable


/**
  * Class for InputHandler. Manages all user input from devices.
  * source for states: https://plus.cs.hut.fi/studio_2/2017/k18/osa01/
  * @param camera
  */
class InputHandler(var camera: Camera) {

  val KEY: mutable.Map[String, Boolean] = mutable.Map[String, Boolean]() // Better to prefix mutable collections to avoid confusion

  private var state: ProgramState = CameraFree

  val lastPoint: Array[Int] = Array[Int](0, 0)

  def setState(state: ProgramState): Unit = {this.state = state}
  def getState: ProgramState = state

  def       click(x: Int, y: Int): Unit =       state.click(x: Int, y: Int)
  def   leftClick(x: Int, y: Int): Unit =   state.leftClick(x: Int, y: Int)
  def  rightClick(x: Int, y: Int): Unit =  state.rightClick(x: Int, y: Int)
  def middleClick(x: Int, y: Int): Unit = state.middleClick(x: Int, y: Int)
  def        drag(x: Int, y: Int, button: MouseButton): Unit = state.drag(x, y, button)
  def keyPressed(key: Key.Value): Unit = state.keyPressed(key)
  def keyReleased(key: Key.Value): Unit = state.keyReleased(key)
  def reactToKeys(delta: Double): Unit = state.reactToKeyInput(delta)


  sealed trait ProgramState {
    def click(x: Int, y: Int): Unit = {}
    def leftClick(x: Int, y: Int): Unit  = {}
    def rightClick(x: Int, y: Int): Unit  = {}
    def middleClick(x: Int, y: Int): Unit  = {}
    def drag(x: Int, y: Int, button: MouseButton): Unit  = {}
    def keyPressed(key: Key.Value): Unit = KEY += key.toString -> true
    def keyReleased(key: Key.Value): Unit = KEY += key.toString -> false
    def reactToKeyInput(delta: Double): Unit  = {}
  }

  case object CameraFree extends ProgramState {
    override def click(x: Int, y: Int): Unit = {
      // Updating last point on every click
      updateLastPoint(x, y)
    }

    override def drag(x: Int, y: Int, button: MouseButton): Unit = {
      val dx = x - lastPoint(0)
      val dy = y - lastPoint(1)
      button match {
        case   `LeftButton` =>
          camera.rotationYaw   += dx/4.0
          camera.rotationPitch -= dy/4.0
        case  `RightButton` =>
          camera.moveXLikeFPS(-dx/10.0)
          camera.moveZLikeFPS(-dy/10.0)
        case `MiddleButton` =>
          camera.moveXLikeFPS(-dx/10.0)
          camera.moveYLikeFPS(-dy/10.0)
      }
      // Updating last point on every drag.
      updateLastPoint(x, y)
    }

    override def reactToKeyInput(delta: Double): Unit =
      for ((x, y) <- KEY if y) {
        x match {
          case "Q" => camera.rotationYaw -= 1
          case "E" => camera.rotationYaw += 1
          case "W" =>     camera.moveZLikeFPS( camera.speed * delta)
          case "S" =>     camera.moveZLikeFPS(-camera.speed * delta)
          case "A" =>     camera.moveXLikeFPS( camera.speed * delta)
          case "D" =>     camera.moveXLikeFPS(-camera.speed * delta)
          case "C" =>     camera.moveYLikeFPS(-camera.speed * delta)
          case "Space" => camera.moveYLikeFPS( camera.speed * delta)
          case "T" =>
            projector.viewerRelationToDisplay = projector.viewerRelationToDisplay.copy(z = projector.viewerRelationToDisplay.z + 5)
          case "G" =>
            projector.viewerRelationToDisplay = projector.viewerRelationToDisplay.copy(z = projector.viewerRelationToDisplay.z + -5)
          case _ =>
      }
    }
  }

  case object CameraDemo extends ProgramState {
    var map: Option[Array[Array[Char]]] = None
    var scale: Double = 0
    private var lastPosition: Vector3 = Vector3(0,0,0)
    def resetCamera: Unit = camera.location = lastPosition
    def savePosition: Unit = lastPosition = camera.location

    override def reactToKeyInput(delta: Double): Unit =
      for ((x, y) <- KEY if y) {
        x match {
          case "T" =>
            projector.viewerRelationToDisplay = projector.viewerRelationToDisplay.copy(z = projector.viewerRelationToDisplay.z + 5)
          case "G" =>
            projector.viewerRelationToDisplay = projector.viewerRelationToDisplay.copy(z = projector.viewerRelationToDisplay.z + -5)
          case _ =>
        }
      }

    override def keyPressed(key: Key.Value): Unit = {
      import Math.floorMod
      super.keyPressed(key)

      key match {
        case Key.Q => camera.rotationYaw -= 90
        case Key.E => camera.rotationYaw += 90
        case Key.W =>
          floorMod(camera.rotationYaw.toInt, 360) match {
            case 0   => move(0, 1)
            case 90  => move(-1, 0)
            case 180 => move(0, -1)
            case _   => move(1, 0)
          }

        case Key.S =>
          floorMod(camera.rotationYaw.toInt, 360) match {
            case 0   => move(0, -1)
            case 90  => move(1, 0)
            case 180 => move(0, 1)
            case _   => move(-1, 0)
          }

        case Key.A =>
          floorMod(camera.rotationYaw.toInt, 360) match {
            case 0   => move(1, 0)
            case 90  => move(0, 1)
            case 180 => move(-1, 0)
            case _   => move(0, -1)
          }

        case Key.D =>
          floorMod(camera.rotationYaw.toInt, 360) match {
            case 0   => move(-1, 0)
            case 90  => move(0, -1)
            case 180 => move(1, 0)
            case _   => move(0, 1)
          }

        case _ =>
      }
    }
    private def move(x: Int, y: Int) = {
      if (map.isDefined && scale != 0) {
        val currentX: Int = currentCoordinates._1
        val currentY: Int = currentCoordinates._2
        val desiredX: Int = currentX + x
        val desiredY: Int = currentY + y

        if (desiredX >= 0 && desiredX < map.get.head.length &&
            desiredY >= 0 && desiredY < map.get.length){
          if (map.get(desiredY)(desiredX) == ' ') {
            map.get(currentY)(currentX) = ' '
            map.get(desiredY)(desiredX) = 'C'
            camera.moveX(x * scale)
            camera.moveZ(y * scale)
          }
        }
      }
    }

    private def currentCoordinates: (Int, Int) = {
      val i: Int = map.get.flatten.indexWhere(c => "NSWEC".contains(c))
      val x: Int = i % map.get.head.length
      val y: Int = i / map.get.length
      (x, y)
    }
  }

  private def updateLastPoint(x: Int, y: Int): Unit = {
    lastPoint(0) = x
    lastPoint(1) = y
  }

  sealed trait MouseButton
  case object LeftButton extends MouseButton
  case object RightButton extends MouseButton
  case object MiddleButton extends MouseButton
}
