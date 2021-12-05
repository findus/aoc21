import scala.::
import scala.io.Source

object Line {
  def fromStrings(start: String, end: String): Line = {
    Line(
      (start.split(",")(0).toInt, start.split(",")(1).toInt),
      (end.split(",")(0).toInt, end.split(",")(1).toInt)
    )
  }
}

case class Line(start: (Int,Int), end: (Int,Int)) {
  def calcPositions(withDiagonals: Boolean): Option[List[(Int,Int)]] = {
    if (start._1 == end._1) {
      Some((Math.min(start._2,end._2) to Math.max(start._2,end._2)).map(e => (start._1, e)).toList)
    } else if (start._2 == end._2) {
      Some((Math.min(start._1,end._1) to Math.max(start._1,end._1)).map(e => (e, start._2)).toList)
    } else if (withDiagonals) {
      val direction_x = if (start._1 - end._1 < 0) { 1 } else { -1 }
      val direction_y = if (start._2 - end._2 < 0) { 1 } else { -1 }
      var solved = false
      var points = List.apply(start)
      var currentPoint = start
      while (!solved) {
        currentPoint = (currentPoint._1 + direction_x, currentPoint._2 + direction_y)
        if (currentPoint == end) {
          points = points.::(currentPoint)
          solved = true
        } else if (currentPoint._1 < 0 || currentPoint._2 < 0 || currentPoint._2 > Math.max(start._2,end._2) || currentPoint._1 > Math.max(start._1,end._1)) {
          return None
        } else {
          points = points.::(currentPoint)
        }
      }
      Some(points)
    } else {
      None
    }
  }
}

object day5 extends App {
  val filename = "src/main/resources/day5"
  val file = Source.fromFile(filename)
  val text = file.getLines();
  val lines = text.map(line => line.split(" -> ")).map(splitted => Line.fromStrings(splitted(0),splitted(1))).toList;
  val positions = lines.flatMap(e => e.calcPositions(false)).flatten.sorted
  val positions_with_diagonals = lines.flatMap(e => e.calcPositions(true)).flatten.sorted
  val grouped = positions.groupBy(f => f).view.mapValues(_.size).filter { case (_,v) => v > 1 }.toMap
  val grouped_diags = positions_with_diagonals.groupBy(f => f).view.mapValues(_.size).filter { case (_,v) => v > 1 }.toMap
  println("Part1: ",grouped.size)
  println("Part2: ",grouped_diags.size)
}