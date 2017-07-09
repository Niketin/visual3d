package visual3d.Model

import java.io.{BufferedInputStream, FileInputStream}
import java.nio.{ByteBuffer, ByteOrder}


/**
  * Object for importing supported file types.
  */
object FileIO {
  /**
    * Imports STL-File. It has to be in binary.
    * Sources:
    *   http://stackoverflow.com/questions/7598135/how-to-read-a-file-as-a-byte-array-in-scala
    *   http://www.fabbers.com/tech/STL_Format
    * @param file
    * @param location
    * @return Does return Some(Mesh) if everything goes well.
    */
  def fromSTLFile(file: String, location: Vector3 = Vector3(0, 0, 0)): Option[Mesh] = {
    try {
      val bis = new BufferedInputStream(new FileInputStream(file))
      val bArray: Array[Byte] = Stream.continually(bis.read).takeWhile(x => x != -1).map(_.toByte).toArray
      if (bArray.take(5).map(_.toChar).mkString("") == "solid") { // if first 5 letters of the file corresponds to ASCII "solid", it is a text file.
        return None
      }

      val bNoHeader: Array[Byte] = bArray.drop(80)
      val facetCount: String = java.lang.Long.toUnsignedString((((((bNoHeader(3) << 8) | bNoHeader(2)) << 8) | bNoHeader(1)) << 8) | bNoHeader(0))
      val bFaces: Array[Byte] = bNoHeader.drop(4)

      val doubles: List[List[Float]] =
        bFaces.grouped(12 * 4 + 2)
          .map(_
            .grouped(12)
            .take(4) // ignores attribute byte count
            .map(_
            .grouped(4)
            .map(x => ByteBuffer.wrap(x).order(ByteOrder.LITTLE_ENDIAN).getFloat())
          )
            .flatten
            .toList
          )
          .toList

      val faces =
        for {
          faceData <- doubles
          v1 = Vertex(faceData(3), faceData(4), faceData(5))
          v2 = Vertex(faceData(6), faceData(7), faceData(8))
          v3 = Vertex(faceData(9), faceData(10), faceData(11))
        } yield Face(v1, v2, v3)

      println(s"Loaded file successfully. \n\tAccording to the file, facet count is: $facetCount")

      Some(Mesh(location, faces))
    }
    catch {
      case ex: Exception => None
    }


  }


  /**
    * Imports APA-File. This is for demo-situations.
    * APA-File includes a 2D-map and a camera's position.
    * @param file
    * @param location
    * @return Mesh, Camera, 2D-map and scale of the Mesh.
    */
  def fromAPAFile(file: String, location: Vector3 = Vector3(0, 0, 0)): Option[(Mesh, Camera, Array[Array[Char]], Double)] = {
    try {
      val fileLines: List[String] = io.Source.fromFile(file).getLines.toList
      val settings: Array[String] = fileLines.head.split(" ")
      val width: Int = settings(0).toInt
      val height: Int = settings(1).toInt
      val wall: Char = settings(2).toString.charAt(0)
      val air: Char = ' '
      val (north, south, west, east) = ('N', 'S', 'W', 'E')
      val groundPlan = Array.tabulate(height, width)((j, k) => fileLines(j + 2)(k))
      val scale = 100


      /**
        * Returns coordinates of wall tile's walls with a condition: wall must not have another wall next to it.
        *
        * @param x
        * @param y
        * @param xMax
        * @param yMax
        * @return
        */
      def wallsOfWallTile(x: Int, y: Int, xMax: Int, yMax: Int): List[(Int, Int, Int, Int)] = { // Tuple content:(x1, y1, x2, y2). They form starting and ending points of a wall
        def isAir(x: Int, y: Int): Boolean = groundPlan(y)(x) != wall // There can also be camera so it's simpler to check just occurance of a wall.

        var walls: List[(Int, Int, Int, Int)] = List()

        if (x == 0 || isAir(x - 1, y)) {
          walls = walls :+ (x, y, x, y + 1)
        }
        if (x == xMax || isAir(x + 1, y)) {
          walls = walls :+ (x + 1, y + 1, x + 1, y)
        }
        if (y == 0 || isAir(x, y - 1)) {
          walls = walls :+ (x + 1, y, x, y)
        }
        if (y == yMax || isAir(x, y + 1)) {
          walls = walls :+ (x, y + 1, x + 1, y + 1)
        }
        walls
      }


      /**
        * Helper method for creating a wall
        *
        * @param x1
        * @param z1
        * @param x2
        * @param z2
        * @return
        */
      def createWall(x1: Int, z1: Int, x2: Int, z2: Int): List[Face] = {
        val v1 = Vertex(x2, 0, z2) * scale
        val v2 = Vertex(x1, 0, z1) * scale
        val v3 = Vertex(x1, 1, z1) * scale
        val v4 = Vertex(x2, 1, z2) * scale
        List(
          Face(v4, v3, v1),
          Face(v2, v1, v3)
        )
      }


      /**
        * Creates a roof for a wall tile.
        * This is currently not supported feature because of the Surface-union-algorithm (polygons cannot currently have any holes).
        *
        * @param x
        * @param y
        * @return
        */
      //TODO surfaces do currently not support complex polygons.
      def createRoof(x: Int, y: Int): List[Face] = {
        val v1 = Vertex(x, 1, y + 1) * scale
        val v2 = Vertex(x + 1, 1, y + 1) * scale
        val v3 = Vertex(x + 1, 1, y) * scale
        val v4 = Vertex(x, 1, y) * scale
        List(
          Face(v1, v2, v4),
          Face(v3, v4, v2)
        )
      }


      val faces: List[Face] = {
        for (row <- 0 until height; column <- 0 until width) yield {
          groundPlan(row)(column) match {
            case `wall` =>
              wallsOfWallTile(column, row, width - 1, height - 1).flatMap(x => createWall(x._1, x._2, x._3, x._4)) //++ createRoof(column, row)
            case _ => List() // Ignore everything else for  now
          }
        }
      }.toList.flatten


      /**
        * Setups Camera to the spot and direction that the APA-file tells. Otherwise set to (0, 0, 0).
        *
        * @return
        */
      def setupCamera: Camera = {
        for (row <- 0 until height; column <- 0 until width) {
          groundPlan(row)(column) match {
            case `north` => return Camera(Vector3(column + 0.5, 0.5, row + 0.5) * scale, yaw = 180.0)
            case `south` => return Camera(Vector3(column + 0.5, 0.5, row + 0.5) * scale)
            case `west` => return Camera(Vector3(column + 0.5, 0.5, row + 0.5) * scale, yaw = 270.0)
            case `east` => return Camera(Vector3(column + 0.5, 0.5, row + 0.5) * scale, yaw = 90.0)
            case _ =>
          }
        }
        new Camera(Vector3(0, 0, 0))
      }

      val mesh = Mesh(Vector3(0, 0, 0), faces)
      Some((mesh, setupCamera, groundPlan, scale))
    }
    catch {
      case _: Exception => None
    }
  }
}

