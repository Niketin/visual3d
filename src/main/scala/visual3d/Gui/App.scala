package visual3d.Gui

import java.awt.Dimension
import javax.swing.BorderFactory
import javax.swing.filechooser.FileNameExtensionFilter

import visual3d.Model.{ Mesh, FileIO}

import scala.swing.{Action, Dialog, FileChooser, Menu, MenuBar, MenuItem}
import javax.swing.SwingUtilities

import visual3d.Model.{Camera, Cuboid, Vector3, World}
import visual3d.Presenter.{InputHandler, Projector}

import scala.swing.{BoxPanel, Graphics2D, MainFrame, Orientation, SimpleSwingApplication}
import scala.swing.event._
import scala.swing._



object App extends SimpleSwingApplication {
  val INIT_SCREEN_WIDTH = 800
  val INIT_SCREEN_HEIGHT = 600
  var windowDimension = new Dimension(INIT_SCREEN_WIDTH, INIT_SCREEN_HEIGHT)

  var camera = new Camera()
  val projector = Projector(INIT_SCREEN_WIDTH, INIT_SCREEN_HEIGHT, camera, None)
  val renderer = Renderer(projector)
  var lastPoint: (Int, Int) = _
  val inputHandler = new InputHandler(camera)

  var  wireFrame: Boolean = false
  var simplified: Boolean = true
  var     planes: Boolean = true
  var  lightMode: Boolean = true // true and false are different light modes.

  var currentFPS: Int = 0
  var currentUPS: Int = 0


  var running = false
  var thread: Thread = new Thread {
    override def run(): Unit = {
      var lastTime: Long = System.nanoTime()
      val maxUPS: Double = 60 // UpdatesPerSecond
      val maxFPS: Double = 60 // FramesPerSecond
      val ns: Double = 1000000000
      val nsPerUpdate = ns / maxUPS
      val nsPerFrame = ns / maxFPS
      var deltaUpdate: Double = 0
      var deltaFrame: Double = 0

      var updates: Int = 0
      var frames: Int = 0
      var loops: Int = 0
      var timer: Long = System.currentTimeMillis()

      while (true) {
        val now: Long = System.nanoTime()
        deltaUpdate += now - lastTime
        deltaFrame += now - lastTime
        lastTime = now

        if (deltaUpdate >= nsPerUpdate) {
          update(deltaUpdate / ns)
          updates += 1
          deltaUpdate -= nsPerUpdate
        }

        if (deltaFrame >= nsPerFrame) {
          render(deltaFrame / ns)
          frames += 1
          deltaFrame -= nsPerFrame
        }

        loops += 1

        if (System.currentTimeMillis() - timer > 1000) {
          timer += 1000
          currentFPS = frames
          currentUPS = updates
          loops = 0
          updates = 0
          frames = 0
        }
        Thread.sleep(1)
      }
    }
  }


  def update(delta: Double): Unit = {
    inputHandler.reactToKeys(delta)
  }
  def render(delta: Double): Unit = {
    canvas.repaint()
  }


  val canvas: BoxPanel = new BoxPanel(Orientation.Horizontal) {
    preferredSize = windowDimension

    listenTo(keys, mouse.clicks, mouse.moves, this)
    import inputHandler.{keyPressed, keyReleased, drag, click, LeftButton, RightButton, MiddleButton}
    reactions += {
      case UIElementResized(source) => projector.updateSize(source.size.width, source.size.height)
      case KeyPressed(_, key, _, _) => keyPressed(key)
      case KeyReleased(_, key, _, _) => keyReleased(key)
      case MousePressed(_, point, _, _, _) => click(point.x, point.y); requestFocus()
      case e @ MouseDragged(_, point, _) =>
        val (x, y) = (point.x, point.y)
        if (isLeftButton(e.peer)) {
          drag(x, y, LeftButton)
        } else if (isRightButton(e.peer)) {
          drag(x, y, RightButton)
        } else if (isMiddleButton(e.peer)) {
          drag(x, y, MiddleButton)
        }
    }

    focusable = true
    requestFocus()

    override def paintComponent(g: Graphics2D): Unit = {
      renderer.render(g, wireFrame, simplified, planes, lightMode)
    }
  }


  val cbFreeCamera = new CheckBox("Free Camera")
  val settingsPanel = new BoxPanel(Orientation.Vertical) {
    val cbWireFrame = new CheckBox("Wire-frame")
    val cbSimplified = new CheckBox("Simplified Polygons")
    val cbPlanes = new CheckBox("Planes")
    val rbLight1 = new RadioButton("Lighting 1")
    val rbLight2 = new RadioButton("Lighting 2")
    val lightingButtons = new ButtonGroup {
      buttons += rbLight1
      buttons += rbLight2
    }

    cbWireFrame.selected = wireFrame
    cbSimplified.selected = simplified
    cbPlanes.selected = planes
    cbSimplified.enabled = cbWireFrame.selected
    if (lightMode) rbLight1.selected = true
    else rbLight2.selected = true

    val renderingSettings = new BoxPanel(Orientation.Vertical) {
      border = BorderFactory.createTitledBorder("Rendering Settings")
      contents += (cbWireFrame, cbSimplified, cbPlanes)
      contents ++= lightingButtons.buttons
      listenTo(cbWireFrame, cbSimplified, cbPlanes, rbLight1, rbLight2)
      reactions += {
        case ButtonClicked(`cbWireFrame`)  =>  wireFrame =  cbWireFrame.selected; cbSimplified.enabled = cbWireFrame.selected
        case ButtonClicked(`cbSimplified`) => simplified = cbSimplified.selected
        case ButtonClicked(`cbPlanes`)     =>     planes =     cbPlanes.selected
        case ButtonClicked(`rbLight1`)     => lightMode =     rbLight1.selected
        case ButtonClicked(`rbLight2`)     => lightMode =     rbLight1.selected
      }
    }

    val speedSlider = new Slider() {
      min = 1
      max = 1000
      value = camera.speed.toInt
      preferredSize = new Dimension(10, 10)
    }


    cbFreeCamera.selected = inputHandler.getState == inputHandler.CameraFree

    val cameraSettings = new BoxPanel(Orientation.Vertical) {
      val cameraSpeed = new Label("Speed") {
        this.horizontalAlignment = Alignment.Center
      }
      border = BorderFactory.createTitledBorder("Camera Settings")
      contents += (cameraSpeed, speedSlider, cbFreeCamera)
      listenTo(speedSlider, cbFreeCamera)
      reactions += {
        case ValueChanged(`speedSlider`) => camera.speed = speedSlider.value
        case ButtonClicked(`cbFreeCamera`) => inputHandler.setState(if (cbFreeCamera.selected) inputHandler.CameraFree else inputHandler.CameraDemo)
      }
    }
    contents += (renderingSettings, cameraSettings)
  }



  val scrollableSettings = new ScrollPane(settingsPanel)
  val splitView =
    new SplitPane(Orientation.Vertical,
      canvas,
      scrollableSettings
    )
  splitView.continuousLayout = true
  splitView.resizeWeight = 1
  splitView.dividerSize = 5


  def top = new MainFrame {
    title = "Visual3D"
    resizable = true

    contents = splitView
    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(Action("Open...") {openFile()})
      }
    }

    peer.setLocationRelativeTo(null)

  }


  /**
    * File chooser dialog method for opening files.
    * Source: http://www.cis.upenn.edu/~matuszek/cis554-2011/Pages/scala-io-code-samples.html
    */
  def openFile(): Unit = {
    val chooser = new FileChooser(new java.io.File("."))
    val stlFilter = new FileNameExtensionFilter("STL File", "stl")
    val apaFilter = new FileNameExtensionFilter("APA File", "apa")
    chooser.peer.addChoosableFileFilter(stlFilter)
    chooser.peer.addChoosableFileFilter(apaFilter)

    val returnValue = chooser.showOpenDialog(null)
    if (returnValue == FileChooser.Result.Approve) {
      val path: String = chooser.selectedFile.getAbsolutePath
      val fileName: String = chooser.selectedFile.getName
      val extension: Option[String] =
        if (fileName.split('.').length > 1) Some(fileName.split('.').last.toLowerCase)
        else None

      if (extension.isDefined) {
        extension.get match {
          case "stl" =>
            val mesh: Option[Mesh] = FileIO.fromSTLFile(path)
            if (mesh.isDefined) {
              projector.world = Some(new World(List(mesh.get)))
              inputHandler.setState(inputHandler.CameraFree)
              cbFreeCamera.selected = true
            } else Dialog.showMessage(null, "Something went wrong while loading the file.\nRemember: ASCII-formated STL File is currently not supported.", title = "Error")

          case "apa" =>
            val mesh: Option[(Mesh, Camera, Array[Array[Char]], Double)] = FileIO.fromAPAFile(path)
            if (mesh.isDefined) {
              projector.world = Some(new World(List(mesh.get._1)))
              projector.camera = mesh.get._2
              camera = mesh.get._2
              inputHandler.camera = mesh.get._2
              inputHandler.setState(inputHandler.CameraDemo)
              inputHandler.CameraDemo.map = Some(mesh.get._3)
              inputHandler.CameraDemo.scale = mesh.get._4
              cbFreeCamera.selected = false
            } else Dialog.showMessage(null, "Something went wrong while loading the file.", title = "Error")

          case _ => Dialog.showMessage(null, "File type is currently not supported.\nSupported files are .STL & .APA", title = "Error")
        }
      }
      else Dialog.showMessage(null, "File name doesn't have any extension.", title = "Error")

    }
  }


  def isLeftButton(mEvent: java.awt.event.MouseEvent): Boolean = SwingUtilities.isLeftMouseButton(mEvent)
  def isRightButton(mEvent: java.awt.event.MouseEvent): Boolean = SwingUtilities.isRightMouseButton(mEvent)
  def isMiddleButton(mEvent: java.awt.event.MouseEvent): Boolean = SwingUtilities.isMiddleMouseButton(mEvent)


  //Starts 'game'loop thread.
  thread.start()
}