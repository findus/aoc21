import day9.cursor

import scala.annotation.tailrec
import scala.io.Source

object day9 extends App {
  val filename = "src/main/resources/day9"
  val file = Source.fromFile(filename)
  val lines = file.getLines().toList

  val listOfLines = lines.map(line => line.map(e => e.asDigit).toList).toList

  def check(cursor: (Int,Int)): (Boolean,Option[(Int,Int)]) = {
    val value = listOfLines(cursor._1)(cursor._2)
    val leftVal = if (cursor._2 != 0)                           { listOfLines(cursor._1)(cursor._2 -1) }   else { -1 }
    val rightVal = if (cursor._2 < listOfLines.head.length -1)  { listOfLines(cursor._1)(cursor._2+ 1) }   else { -1 }
    val topVal = if (cursor._1 != 0)                            { listOfLines(cursor._1 - 1)(cursor._2)}   else { -1 }
    val botVal = if (cursor._1 < listOfLines.length - 1)        { listOfLines(cursor._1 + 1)(cursor._2) }  else { -1 }
    val result = (value < leftVal || leftVal == -1) && (value < rightVal || rightVal == -1 ) && (value < topVal || topVal == -1) && (value < botVal || botVal == -1)

/*    if (result) {
      println(s"   $topVal ")
      println(s" $leftVal $value $rightVal ")
      println(s"  $botVal ")
    }*/

    (result,Some(cursor))
  }

  def getNeighboursBelow9(cursor: (Int,Int), ignore: List[(Int,Int)]): List[(Int,Int)] = {

    val leftVal = if (cursor._2 != 0)                           { Some((listOfLines(cursor._1)(cursor._2 -1), (cursor._1,cursor._2 -1))) } else {  None }
    val rightVal = if (cursor._2 < listOfLines.head.length -1)  { Some((listOfLines(cursor._1)(cursor._2+ 1), (cursor._1,cursor._2 + 1))) } else { None }
    val topVal = if (cursor._1 != 0)                            { Some((listOfLines(cursor._1 - 1)(cursor._2),(cursor._1 - 1,cursor._2))) } else { None }
    val botVal = if (cursor._1 < listOfLines.length - 1)        { Some((listOfLines(cursor._1 + 1)(cursor._2),(cursor._1 + 1,cursor._2)))} else { None }
    val list = List(leftVal,rightVal,topVal,botVal).filter(e => e.isDefined).filter(e => e.get._1 >= 0 && e.get._1 < 9).map(e => e.get).filter(e => {
      !ignore.contains(e._2)
    }).filter(pp => pp._2 != cursor)

    val e = list.foldLeft(List.empty[(Int,Int)])((prev,data) => getNeighboursBelow9(data._2, (prev.++(ignore)).distinct.++(list.map(e=>e._2)).distinct))
    e.++(ignore).distinct
  }


  var cursor = (0,0)
  var yLen = listOfLines.head.length - 1
  var xLen = listOfLines.length - 1

  listOfLines.map(line => println(line))

  val e = for {
    x <- 0 to xLen
    y <- 0 to yLen
  } yield {
    if (check((x,y))._1) { Some(1 + listOfLines(x)(y)) } else { None }
  }
  println("Part1: ",e.filter(e => e.isDefined).map(e => e.get).sum)

  val f = for {
    x <- 0 to xLen
    y <- 0 to yLen
  } yield {
    val c = check((x,y))
    if (c._1) { c._2 } else { None }
  }
  val basinscenter = f.filter(e => e.isDefined).map(e => e.get)

  val test = basinscenter.head
  val test2 = basinscenter.map(entry => getNeighboursBelow9(entry,List(entry)))
  println(test2.map(e => e.length).sorted.reverse.slice(0,3).product)
}
