import scala.io.Source

object day14 extends App {

  val filename = "src/main/resources/day14"
  val file = Source.fromFile(filename)
  val lines = file.getLines().toList
  val (a,b) = lines.filter(line => line.nonEmpty).partition(line => line.contains("->"))
  val polymerTemplate = b.head.toCharArray
  val insertionRules = a.map(line => (line.split(" -> ")(0),line.split(" -> ")(1))).toMap

  val steps = polymerTemplate.sliding(2).map(step => step.mkString("")).toList

  private def fold(to: Int) = {
    val result = (1 to to).foldLeft(steps)((prev, data) => {
      val test = prev.map(step => step(0) + insertionRules.getOrElse(step, "") + step(1))
      val test2 = test.map(str => str.dropRight(1)).mkString("") + test.tail.last.last.toString
      val list = test2.sliding(2).toList
      println(data)
      list
    })

    val resultString =  result.map(str => str.dropRight(1)).mkString("") + result.tail.last.last.toString
    val mapped = resultString.groupBy(f => f).view.mapValues(e => e.length).toMap.values.toList.sorted
    mapped.last - mapped.head
  }

  // Part1
  println(fold(10))

  // Part1
  println(fold(40))
}
