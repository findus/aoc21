import scala.io.Source

object day7 extends App {
  val filename = "src/main/resources/day7"
  val file = Source.fromFile(filename)
  val crabPosition = file.getLines().toList.head.split(",").map(e => e.toInt)
  println(crabPosition.toList)
  val e = (0 to crabPosition.length).map(e => crabPosition.map(cp => {
    (cp - e).abs
  })).toList
  println(e.map(f => f.sum).min)
}