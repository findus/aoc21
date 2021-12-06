import day1.filename

import scala.io.Source

object day3 extends App {

  val filename = "src/main/resources/day3"
  val file = Source.fromFile(filename)
  val lines = file.getLines()

  def part1: Unit = {


    def getMostCommonBit(position: Int): Int = {
      lines.map(e => e(position)).partition(e => e != 0) match {
        case (ones,zeros) if ones.size > zeros.size => 1
        case _ => 0
      }
    }

    val d = 0 to 5 map(count => getMostCommonBit(count))

    println(d)

  }

  part1

  file.close()

}
