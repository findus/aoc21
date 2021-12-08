import scala.io.Source

case class Segment(one: String, four: String, seven: String, eight: String ) {
  private var top = one.intersect(eight).intersect(four).intersect(seven)
  private var rightBottomRightTop = seven.diff(four)
  private var middleTopLeft =  four.diff(seven).diff(one).diff(top)
  private var bottomLeft = eight.diff(seven).diff(one).diff(four)

  def getFor(str: String): (String,Int) = {
     val d = str match {
       case s if s.length == 6 && bottomLeft.intersect(s).length == 2 && seven.intersect(s).length == 2 => 6
       case s if s.length == 6 && seven.intersect(s).length == 3 && bottomLeft.intersect(s).length == 1 => 9
       case s if s.length == 5 && middleTopLeft.intersect(s).length == 2 && one.intersect(s).length == 1 => 5
       case s if s.length == 6 && eight.diff(s).length == 1 && bottomLeft.intersect(s).length == 2 => 0
       case s if s.length == 2 => 1
       case s if s.length == 5 && s.intersect(middleTopLeft).length == 1 && s.intersect(seven).length == 3 => 3
       case s if s.length == 5 && s.intersect(middleTopLeft).length == 1 => 2
       case s if s.length == 4 => 4
       case s if s.length == 3 => 7
       case s if s.length == 7 => 8
       case _ => -1
    }
    (str.sorted,d)
  }
}

object day8 extends App {
  val filename = "src/main/resources/day8"
  val file = Source.fromFile(filename)
  val map = Seq((2 -> 1), (4 -> 4), (3 -> 7), (7 -> 8)).toMap

  def getNr(str: List[String], int: Int): String = {
    str.filter(e => e.length == int).head
  }

  val lines = file.getLines().toList
  val output = lines.map(e => e.split('|')(1).trim.split(" ").toList).toList
  val signals = lines.map(e => e.split('|')(0).trim.split(" ").toList).toList
  val count = output.map(o => map.getOrElse(o.length, -1)).count(d => d != -1)
  val count2 = signals.map(e => {
    val s = Segment(getNr(e, 2), getNr(e, 4), getNr(e, 3), getNr(e, 7))
    (e, s)
  }).map { case (list, segment) => list.map(entry => segment.getFor(entry)) }
  val e = count2.map(d => d.toMap)
  println(e)
  println(output.zip(e).map(o => o._1.map(x => o._2(x.sorted))).map(e => e.mkString("").toInt).toList.sum)
}
