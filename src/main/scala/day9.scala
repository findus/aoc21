import scala.io.Source

object day9 extends App {
  val filename = "src/main/resources/day9"
  val file = Source.fromFile(filename)
  val lines = file.getLines().toList

  def orElse(e: Int) = -1

  def upperHigher(passedHeight: Int, upperList: List[Int], position: Int) = {
    upperList(position) > passedHeight
  }

  def lowerHigher(passedHeight: Int, lowerList: List[Int], position: Int) = {
    lowerList(position) > passedHeight
  }

  def lowerSameList(passedPosition: Int, list: List[Int]) = {
    list.applyOrElse(passedPosition - 1, orElse) > list.applyOrElse(passedPosition, orElse) && passedPosition % 10 != 0 &&
      list.applyOrElse(passedPosition, orElse) < list.applyOrElse(passedPosition + 1, orElse)
  }

  val listOfLines = lines.map(line => line.map(e => e.asDigit).toList).toList


  def check(cursor: (Int,Int)) = {
    val value = listOfLines(cursor._1)(cursor._2)
    val leftVal = if (cursor._2 != 0)                           { listOfLines(cursor._1)(cursor._2 -1) }   else { -1 }
    val rightVal = if (cursor._2 < listOfLines.head.length -1)  { listOfLines(cursor._1)(cursor._2+ 1) }   else { -1 }
    val topVal = if (cursor._1 != 0)                            { listOfLines(cursor._1 - 1)(cursor._2)}   else { -1 }
    val botVal = if (cursor._1 < listOfLines.length - 1)        { listOfLines(cursor._1 + 1)(cursor._2) }  else { -1 }
    val result = (value < leftVal || leftVal == -1) && (value < rightVal || rightVal == -1 ) && (value < topVal || topVal == -1) && (value < botVal || botVal == -1)

    if (result) {
      println(s"   $topVal ")
      println(s" $leftVal $value $rightVal ")
      println(s"  $botVal ")
    }

    result
  }


  var cursor = (0,0)
  var yLen = listOfLines.head.length - 1
  var xLen = listOfLines.length - 1

  listOfLines.map(line => println(line))

  val e = for {
    x <- 0 to xLen
    y <- 0 to yLen
  } yield {
    if (check((x,y))) { Some(1 + listOfLines(x)(y)) } else { None }
  }


  println(e.filter(e => e.isDefined).map(e => e.get).sum)
}
