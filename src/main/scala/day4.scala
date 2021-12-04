import day4.won

import scala.io.Source
import scala.collection.immutable.SortedMap

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

  implicit def ordering[A <: Seq[String]]: Ordering[A] = new Ordering[A] {
    override def compare(x: A, y: A): Int = {
      x.size.compareTo(y.size)
    }
  }

  val won = input
    .foldLeft(SortedMap.empty[Seq[String],Board],Seq.empty[String],boards)((prev, data) => {
      val i: Seq[String] = prev._2 ++ Seq(data)
      val (boardsThatWon,remaining) = prev._3.partition(board => board.won(i))
      val wonBoards = boardsThatWon.foldLeft(prev._1)((prev,data) => prev ++ SortedMap.apply((i,data)))
      (wonBoards,i,remaining)
    })

  def part1() = {
    val sum = won._1.head._2.unmarkedSum(won._1.head._1)
    val winningNumber = won._1.head._1.last.toInt
    println(s"Part1: Sum: ${sum} WinningNbr: ${winningNumber} => ${sum * winningNumber}")
  }

  def part2() = {

    implicit def ordering[A <: Seq[String]]: Ordering[A] = new Ordering[A] {
      override def compare(x: A, y: A): Int = {
        x.size.compareTo(y.size)
      }
    }

    won._1.foreach(entry => {
      val sum = entry._2.unmarkedSum(entry._1)
      val winningNumber = entry._1.last.toInt
      println(s"Part2: Sum: ${sum} WinningNbr: ${winningNumber} => ${sum * winningNumber}")
    })
  }

  part1()
  part2()

}