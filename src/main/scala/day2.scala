import day1.filename

import scala.io.Source

object day2 extends App {

  part1
  part2

  private def part1 = {
    val filename = "src/main/resources/day2"

    var horizontalPosition = 0;
    var depth = 0;

    val file = Source.fromFile(filename)
    val commands = file.getLines().map { input => input.split(" ") }
    commands.foreach(e => {
      val command = e(0)
      val data = Integer.valueOf(e(1))

      command match {
        case "forward" => horizontalPosition += data
        case "up" => depth -= data
        case "down" => depth += data
      }
    })

    println(horizontalPosition * depth)
  }

    private def part2 = {
      val filename = "src/main/resources/day2"

      var aim = 0;
      var depth = 0;
      var horizontalPosition = 0;


      val file = Source.fromFile(filename)
      val commands = file.getLines().map { input => input.split(" ") }
      commands.foreach(e => {
        val command = e(0)
        val data = Integer.valueOf(e(1))

        command match {
          case "forward" => {
            horizontalPosition += data
            depth += (aim*data)
          }
          case "up" => aim -= data
          case "down" => aim += data
        }
      })

      println(horizontalPosition * depth)
  }


}
