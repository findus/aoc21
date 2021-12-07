import scala.io.Source

object day7 extends App {
  val filename = "src/main/resources/day7"
  val file = Source.fromFile(filename)
  val crabPosition = file.getLines().toList.head.split(",").map(e => e.toInt)
  println((0 to crabPosition.length).map(e => crabPosition.map(cp => (cp - e).abs)).map(f => f.sum).min)
  println((0 to crabPosition.length).map(e => crabPosition.map(cp => (1 until (cp - e).abs + 1).sum)).map(f => f.sum).min)
}