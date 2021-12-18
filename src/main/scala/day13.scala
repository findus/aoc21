import scala.io.Source

case class FoldInstruction(along: String, at: Int)
case class Coordinate(x: Int, y: Int)

object day13 extends App {
  val filename = "src/main/resources/day13"
  val file = Source.fromFile(filename)
  val lines = file.getLines().toList
  val (fi,coords) = lines.filter(line => line.nonEmpty).partition(string => string.startsWith("fold"))

  val foldInstructions = fi.map(string => {
    val data = string.replace("fold along ","").split("=")
    FoldInstruction(data(0),data(1).toInt)
  })

  def fold(instruction: FoldInstruction,coordinates: List[Coordinate]) = {
    if (instruction.along == "y") {
      val at = instruction.at
      coordinates.foldLeft(List.empty[Coordinate])((prev,data) => {
        if (at - data.y > 0) {
          prev.::(data)
        } else {
          val amount = if  (at - data.y == -1) { -2 } else { ((at - data.y) * 2)}
          val newY = data.y + amount
          prev.::(Coordinate(data.x, newY))
        }
      }).distinct
    } else {
      val at = instruction.at
      coordinates.foldLeft(List.empty[Coordinate])((prev,data) => {
        if (at - data.x > 0) {
          prev.::(data)
        } else {
          val amount = if  (at - data.x == -1) { -2 } else { ((at - data.x) * 2)}
          val newX = data.x + amount
          prev.::(Coordinate(newX, data.y))
        }
      }).distinct
    }
  }

  val coordinates = coords.map(coord => { Coordinate(coord.split(",")(0).toInt, coord.split(",")(1).toInt) })

  println(coordinates)
  println(s"Dots before fold: ${coordinates.length}" )

  val height = coordinates.map(c => c.y).max
  val width = coordinates.map(c => c.x).max

  val firstFold = foldInstructions.head

  val folded = fold(firstFold,coordinates)

  //Part1
  println(folded.length)
  println(folded)

  //Part2
  val folded2 = foldInstructions.foldLeft(coordinates)((prev,data) => fold(data, prev))
  val maxY = folded2.map(e => e.y).max
  val maxX = folded2.map(e => e.x).max

  var lastY = 0
  for {
    y <- (0 to maxY)
    x <- (0 to maxX)
  } {
    if (lastY != y) {
      println("")
      lastY = y
    }
    if (folded2.contains(Coordinate(x,y))) {
      print("X")
    } else {
      print(" ")
    }
  }
  println(folded2.length)
  println(folded2)
}
