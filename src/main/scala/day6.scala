import scala.io.Source

case class Lanternfish(var state: Int) {
  def life(): List[Lanternfish] = {
    state -= 1;
    if (state < 0) {
      state = 6
      return List(Lanternfish(8));
    }
    List.empty
  }

  override def toString(): String = {
    state.toString
  }
}

object day6 extends App {
  val filename = "src/main/resources/day6"
  val file = Source.fromFile(filename)
  val fishes = file.getLines().toList.head.split(",").map(number => Lanternfish(number.toInt));
  println((1 to 80).foldLeft(fishes)((prev, _) => prev ++ prev.flatMap(fishes => fishes.life())).toList.size)
 // println((1 to 256).foldLeft(fishes)((prev, _) => prev ++ prev.flatMap(fishes => fishes.life())).toList.size)

}


