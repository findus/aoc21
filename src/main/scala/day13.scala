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

  val coordinates = coords.map(coord => { Coordinate(coord.split(",")(0).toInt, coord.split(",")(1).toInt) })

  println(coordinates)
  println(s"Dots before fold: ${coordinates.length}" )

  val height = coordinates.map(c => c.y).max
  val width = coordinates.map(c => c.x).max

  val firstFold = foldInstructions.head

  val folded = if (firstFold.along == "y") {
    val at = firstFold.at
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
    val at = firstFold.at
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

  println(folded.length)
  println(folded)


}
