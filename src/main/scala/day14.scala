import scala.io.Source

object day14 extends App {

  val filename = "src/main/resources/day14"
  val file = Source.fromFile(filename)
  val lines = file.getLines().toList
  val (a,b) = lines.filter(line => line.nonEmpty).partition(line => line.contains("->"))
  val polymerTemplate = b.head.toCharArray.map(e => e.toString)
  val insertionRules = a.map(line => (line.split(" -> ")(0),line.split(" -> ")(1))).toMap

  val steps = polymerTemplate.sliding(2).map(step => step.mkString("")).toList

  val map: Map[String,Long] = steps.groupBy(f => f).view.mapValues(e => e.length.toLong).toMap
  val charCount: Map[String,Long] = polymerTemplate.groupBy(f => f).view.mapValues(e => e.length.toLong).toMap


  Long.MaxValue
  private def fold2(to: Int) = {
    val result = (1 to to).foldLeft((map,charCount))((prev,data) => {
      var ck = prev._2
      val e = prev._1.toList.flatMap(entry => {
        val newChar = insertionRules.getOrElse(entry._1,"")
        if (prev._2.contains(newChar)) {
          ck = ck.updated(newChar, ck(newChar) + entry._2.toLong)
        } else {
          val d = Seq(newChar -> entry._2.toLong).toMap
          ck = ck.++(d)
        }
        val news = (entry._1(0) + newChar + entry._1(1)).toCharArray.sliding(2).map(e => e.mkString("")).toList
        val news2: List[(String,Long)] = news.map { case (a) => (a,entry._2)  }
        news2
      })

      val aggregated = e.foldLeft(List.empty[(String,Long)])((prev, data) => {
        if (prev.exists(d => d._1.contains(data._1))) {
          val newcount = prev.filter(d => d._1.contains(data._1)).head._2 + data._2
          prev.updated(prev.indexWhere(d => d._1.equals(data._1)), (data._1,newcount ))
        } else {
          prev.::(data)
        }
      }).toMap

      (aggregated,ck)
    })

    val sortedList = result._2.values.toList.sorted
    sortedList.last - sortedList.head
  }

  private def fold(to: Int) = {
    val result = (1 to to).foldLeft(steps)((prev, data) => {
      val test = prev.map(step => step(0) + insertionRules.getOrElse(step, "") + step(1))
      val test2 = test.map(str => str.dropRight(1)).mkString("") + test.tail.last.last.toString
      val list = test2.sliding(2).toList
      list
    })

    val resultString =  result.map(str => str.dropRight(1)).mkString("") + result.tail.last.last.toString
    val mapped = resultString.groupBy(f => f).view.mapValues(e => e.length).toMap.values.toList.sorted
    mapped.last - mapped.head
  }

  // Part1
  println(fold(10))

  // Part1
  println(fold2(40))
}
