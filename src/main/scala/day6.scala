import scala.io.Source

object day6 extends App {
  val filename = "src/main/resources/day6"
  val file = Source.fromFile(filename)
  val numbers = file.getLines().toList.head.split(",")
  def partx() = {
    val o = numbers.groupBy(f => f.toLong).view.mapValues(_.size.toLong).toMap
    def plusDay(map: Map[Long,Long]): Map[Long,Long] = {
      val e = map.map(entry => (entry._1 - 1, entry._2)).toMap
      val reproduce = e.getOrElse(-1L,0L)
      val newMap = e.filter(e => e._1 != -1 && e._1 != 6).+ ((8,reproduce))
      val newnewMap = newMap.+((6L,e.getOrElse(6,0L)+e.getOrElse(-1,0L)))
      newnewMap
    }
    println((1 to 80).foldLeft(o)((prev,_) => plusDay(prev)).values.sum)
    println((1 to 256).foldLeft(o)((prev,_) => plusDay(prev)).values.sum)
  }
  partx()
}


