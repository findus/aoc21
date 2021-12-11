import day9.lines

import scala.io.Source

object day11 extends App {
  val filename = "src/main/resources/day11"
  val file = Source.fromFile(filename)
  val lines = file.getLines().toList
  var flashes: Int = 0

  val listOfLines = lines.map(line => line.map(e => e.asDigit).toList).toList

  def getPositionOf10s(list: List[List[Int]]): List[(Int,(Int,Int))] = {
    list.zipWithIndex
      .map{ case (l,y) => l.zipWithIndex
        .map{case (data,x) => (data,(x,y))}
      }.flatten
      .filter(e => e._1 >= 10 && e._1 < 50)
  }

  def getPositionsAround10s(list: List[(Int,(Int,Int))]): List[(Int,Int)] = {
    list.map(entry => entry._2).flatMap(entry => {
      val topLeft     = (entry._1 - 1, entry._2 - 1)
      val top         = (entry._1    , entry._2 - 1)
      val topRight    = (entry._1 + 1, entry._2 - 1)
      val right       = (entry._1 + 1, entry._2    )
      val bottomRight = (entry._1 + 1, entry._2 + 1)
      val bottom      = (entry._1    , entry._2 + 1)
      val bottomLeft  = (entry._1 - 1, entry._2 + 1)
      val left        = (entry._1 - 1, entry._2)
      List(topLeft,top, topRight, right, bottomRight, bottom, bottomLeft, left)
        .filter { case(x,y) => x >= 0 && x < listOfLines.head.length && y >= 0 && y < listOfLines.length}
    }).sorted
  }

  def incrementPosition(list: List[List[Int]], position: (Int,Int)): List[List[Int]] = {
    val oldVal = list(position._2)(position._1)
    list.updated(position._2,list(position._2).updated(position._1,oldVal + 1))
  }

  def markPositionAsUsedPosition(list: List[List[Int]], position: (Int,Int)): List[List[Int]] = {
    val oldVal = list(position._2)(position._1)
    list.updated(position._2,list(position._2).updated(position._1,oldVal + 50))
  }

  def calculateFlashingOctopusses(list:List[List[Int]]): List[List[Int]] = {
    var l = list
    while (getPositionOf10s(l).nonEmpty) {
      val posOf10s = getPositionOf10s(l)
      flashes = flashes + posOf10s.length
      val aroundPosistions = getPositionsAround10s(posOf10s)
      // "Mark" all position that got already processed by incrementing them by one
      l = posOf10s.map(e => e._2).foldLeft(l)((prev,data) => markPositionAsUsedPosition(prev,data))
      l = aroundPosistions.foldLeft(l)((prev,data) => incrementPosition(prev,data))
      l
    }

    l.map(e => e.map(f => if (f > 9) { 0 } else { f }))
  }

  def increment(list: List[List[Int]]): List[List[Int]] = {
    list.map(row => row.map(nmbr => nmbr + 1))
  }

  val rounds = (1 to 100).foldLeft(listOfLines)((prev,count) => {
    val incremented = increment(prev)
    val newMap = calculateFlashingOctopusses(incremented)
    println("step,", count)
    print(newMap)
    newMap
  }).toList

  println(flashes)


  private def print(incremented: List[List[Int]]) = {
    incremented.foreach(r => {
      println(r.map(e=> e.toString.padTo(2,' ')).mkString(" "))
    })
    println("")
  }
}
