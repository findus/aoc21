import day1.filename
import scala.io.Source

object day3 extends App {

  val filename = "src/main/resources/day3"
  val file = Source.fromFile(filename)
  val lines = file.getLines().toList

  def getMostCommonBit(lines: List[String], position: Int): Int = {
    lines.map(e => e(position)).map(_.asDigit).partition(e => e != 0) match {
      case (ones,zeros) if ones.size > zeros.size || ones.size == zeros.size => 1
      case _ => 0
    }
  }

  def getLeastCommonBit(lines: List[String], position: Int): Int = {
    lines.map(e => e(position)).map(_.asDigit).partition(e => e == 0) match {
      case (zeros,ones) if zeros.size > ones.size => 1
      case (zeros,ones) if ones.size == zeros.size => 0
      case _ => 0
    }
  }

  def part1: Unit = {

    val most = (0 to 11) map { count => getMostCommonBit(lines, count) }
    val least = (0 to 11) map { count => getLeastCommonBit(lines, count) }
    val gamma = Integer.parseInt(most.mkString,2)
    val epsilon = Integer.parseInt(least.mkString,2)

    println(most.mkString, least.mkString)
    println(gamma,epsilon, gamma*epsilon)
  }

  def findData(func: (List[String],Int) => Int): String = {

    var lifeSupport = lines;
    var counter = 0;

    while (lifeSupport.size > 1) {
      val mostCommon = func.apply(lifeSupport,counter)
      lifeSupport = lifeSupport.filter(l => l(counter).asDigit == mostCommon)
      counter += 1
    }

    lifeSupport.head
  }

  def part2: Unit = {
    val oxygen = findData(getMostCommonBit)
    val co2Scrubber = findData(getLeastCommonBit)
    println(Integer.parseInt(oxygen,2) * Integer.parseInt(co2Scrubber,2))
  }

  part1
  part2

  file.close()

}
