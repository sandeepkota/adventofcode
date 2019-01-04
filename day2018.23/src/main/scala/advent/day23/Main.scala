package advent.day23

import java.io.File
import scala.io.Source

case class Position(x: Int, y: Int, z: Int)
case class PositionAndRadius(position: Position, radius: Int)

object Main {
  var inputPositions: Seq[PositionAndRadius] = Seq()

  def main(args: Array[String]): Unit = {
    readInputValues()
    val maxRangePosition = inputPositions.reduceLeft((a, b) => findTheMax(a, b))

    println("position which has largest signal radius: " + maxRangePosition)
    println("largest signal radius" + findInRangePositions(maxRangePosition).size)
  }

  def readInputValues() {
    val f = new File(getClass.getClassLoader.getResource("input.txt").getPath)
    Source.fromFile(f).getLines().foreach { line =>
      parseLine(line) match {
        case Seq(a, b, c, d) => inputPositions = inputPositions :+ PositionAndRadius(Position(a, b, c), d)
        case a: Any => a.foreach(println)
      }
    }
  }

  def parseLine(l: String): Seq[Int] = {
    l.drop(5).replace(">, r=", ",").split(",").map(_.toInt).toSeq
  }

  def findTheMax(x: PositionAndRadius, y: PositionAndRadius): PositionAndRadius = {
    if (findInRangePositions(x).size <= findInRangePositions(y).size) y
    else x
  }

  def findInRangePositions(pos: PositionAndRadius): Seq[PositionAndRadius] = {
    inputPositions.filter(findManhattanDistance(pos, _) <= pos.radius)
  }

  def findManhattanDistance(pos1: PositionAndRadius, pos2: PositionAndRadius): Int = {
    Math.abs(pos1.position.x - pos2.position.x) +
      Math.abs(pos1.position.y - pos2.position.y) +
      Math.abs(pos1.position.z - pos2.position.z)
  }

}
