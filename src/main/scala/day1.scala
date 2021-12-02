import scala.io.Source

object day1 extends App {

  val filename = "src/main/resources/input"

  def main(): Unit = {
    val file = Source.fromFile(filename)
    val numbers = file.getLines().map(input => Integer.valueOf(input)).toList
    val pairs = numbers.zip(numbers.drop(1))
    val amount = pairs.count { case (x, y) => x < y }
    println(amount)
    file.close();
  }

  def main2(): Unit = {
    val file = Source.fromFile(filename)
    val numbers = file.getLines().map(input => Integer.valueOf(input)).toList

    var index = 0
    val original = numbers.zipWithIndex.map{ case (entry,index) =>
      if ((numbers.size - 3) >= index) {
        numbers(index) + numbers(index + 1) + numbers(index + 2)
      } else {
        -1
      }
    }.filter( e => e > 0)

    val e = original.zip(original.drop(1)).map { case (a,b) => a < b }.filter(e => e).toList.size

    println(e)

    file.close();
  }

  main();
  main2();
}