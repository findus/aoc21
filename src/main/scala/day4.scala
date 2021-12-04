import scala.io.Source

case class Board(data: Seq[Array[String]]) {

  if (data.exists(e => e.length > 5 || e.length < 5)) {
    throw new IllegalStateException("Board data not correct")
  }

  private def rotate(): Seq[Array[String]] = {
    (0 to 4).map { value =>
      data.map(entry => entry(value)).toArray
    }
  }

  def won(withNumbers: Seq[String]): Boolean = {

    def numberAvailable(row: String) = {
      withNumbers.contains(row)
    }

    def rowContains(array: Array[String]): Boolean  = {
      array.foldLeft(true)((prev, data) => numberAvailable(data) && prev)
    }

    def rowsContains(): Boolean = {
      data.foldLeft(false)((prev,data) => rowContains(data) || prev)
    }

    def columnsContains(): Boolean = {
      rotate().foldLeft(false)((prev,data) => rowContains(data) || prev)
    }

    rowsContains() || columnsContains()
  }

  def unmarkedSum(input: Seq[String]): Integer = {
    val e = data
      .reduce((prev,data) => prev.concat(data))
      .filter(char => !input.contains(char))
      .map(char => Integer.valueOf(char))
    if (e.size == 0) { -1 } else { e.reduce(_ + _) }
  }
}


object day4 extends App {

  val filename = "src/main/resources/day4"
  val file = Source.fromFile(filename)
  val lines = file.getLines();

  val input = lines.next().split(",").toList
  lines.next()

  val boards = lines.sliding(5,6).map(strs => strs.map(str => str.trim.split("  ?"))).map(strs => Board(strs)).toList

  val won = input
    .foldLeft(Seq.empty[Board],Option.empty[Seq[String]],Seq.empty[String],Option.empty[Board])((prev,data) => {
      val i: Seq[String] = prev._3 ++ Seq(data)
      val boardsThatWon = boards.filter(board => board.won(i))
      val winningNmbr = if (prev._1.isEmpty && boardsThatWon.nonEmpty) {Some(i)} else {prev._2}
      val winningBoard = if (prev._4.isEmpty && boardsThatWon.nonEmpty) {Some(boardsThatWon.head)} else {prev._4}
      (boardsThatWon,winningNmbr, i, winningBoard)
    })


  val sum = won._4.get.unmarkedSum(won._2.get)
  println(s"Sum: ${sum} WinningNbr: ${won._2.get.last} => ${sum * won._2.get.last.toInt}")

}