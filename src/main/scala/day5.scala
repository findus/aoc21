import scala.io.Source

object Line {
  def fromStrings(start: String, end: String): Line = {
    Line(
      (start.split(",")(0).toInt, start.split(",")(1).toInt),
      (end.split(",")(0).toInt, end.split(",")(1).toInt)
    )
  }
}

case class Line(start: (Int,Int), end: (Integer,Integer)) {
  def calcPositions(): Option[List[(Int,Int)]] = {
    if (start._1 == end._1) {
      Some((Math.min(start._2,end._2) to Math.max(start._2,end._2)).map(e => (start._1, e)).toList)
    } else if (start._2 == end._2) {
      Some((Math.min(start._1,end._1) to Math.max(start._1,end._1)).map(e => (e, start._2)).toList)
    }  else {
      None
    }
  }
}

object day5 extends App {

  val filename = "src/main/resources/day5"
  val file = Source.fromFile(filename)
  val text = file.getLines();
  val lines = text.map(line => line.split(" -> ")).map(splitted => Line.fromStrings(splitted(0),splitted(1))).toList;
  val positions = lines.flatMap(e => e.calcPositions()).flatten.sorted
  val grouped = positions.groupBy(f => f).view.mapValues(_.size).filter { case (_,v) => v > 1 }.toMap
  println(grouped.size)
}