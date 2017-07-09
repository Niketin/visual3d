package visual3d.Model

import scala.math.{toRadians, cos, sin}
import visual3d.Utils._


/**
  * Camera class. This is user's eyes.
  * Picture is produced from the camera's view in other classes.
  * @param location Has default location of (0, 0, 0)
  */
class Camera(var location: Vector3 = Vector3(0, 0, 0)) {

  var speed: Double = 50 // Units per second

  private var _rotationYaw: Double = 0
  private var _rotationPitch: Double = 0


  /**
    * Returns yaw.
    * @return
    */
  def rotationYaw: Double = _rotationYaw


  /**
    * Returns pitch.
    * @return
    */
  def rotationPitch: Double = _rotationPitch


  /**
    * Sets corresponding rotation to the degree value.
    * @param degrees In degrees.
    */
  def rotationYaw_=(degrees: Double): Unit = _rotationYaw = degrees


  /**
    * Sets pitch.
    * @param degrees In degrees. Result will be limited: [-90.0, 90.0].
    */
  def rotationPitch_=(degrees: Double): Unit = _rotationPitch = clamp(degrees, -90.0, 90.0)


  /**
    * Increases yaw.
    * @param degrees In degrees.
    */
  def rotationYaw_+=(degrees: Double): Unit = _rotationYaw += degrees


  /**
    * Increases pitch.
    * @param degrees In degrees. Result will be limited: [-90.0, 90.0].
    */
  def rotationPitch_+=(degrees: Double): Unit = _rotationPitch = clamp(_rotationPitch + degrees, -90.0, 90.0)


  /**
    * First Person Shooter -style movement, thanks to trigonometric functions.
    * Camera's pitch does not affect movement.
    * Camera will be moved in its own coordinate system's x, y & z directions and that is kinda translated to world's coordinate system.
    * @param amount Moves camera in the corresponding direction for the given amount.
    */
  def moveXLikeFPS(amount: Double): Unit = { location = location.copy(x = location.x + amount*cos(toRadians(rotationYaw)), z = location.z + amount*sin(toRadians(rotationYaw))) }
  def moveYLikeFPS(amount: Double): Unit = { location = location.copy(y = location.y + amount) }
  def moveZLikeFPS(amount: Double): Unit = { location = location.copy(x = location.x + amount*sin(toRadians(-rotationYaw)), z = location.z + amount*cos(toRadians(-rotationYaw))) }


  /**
    * Moves camera in world's coordinate system regardless of the camera's rotation.
    * @param amount Moves camera in the corresponding direction for the given amount.
    */
  def moveX(amount: Double): Unit = { location = location + Vector3(amount, 0, 0) }
  def moveY(amount: Double): Unit = { location = location + Vector3(0, amount, 0) }
  def moveZ(amount: Double): Unit = { location = location + Vector3(0, 0, amount) }

}

/**
  * Camera's companion object.
  */
object Camera {

  /**
    * Factory method for Camera.
    * @param location
    * @param yaw
    * @param pitch
    * @return
    */
  def apply(location: Vector3 = Vector3(0, 0, -100), yaw: Double = 0.0, pitch: Double = 0.0): Camera = {
    val camera = new Camera(location)
    camera.rotationPitch = pitch
    camera.rotationYaw = yaw
    camera
  }
}
