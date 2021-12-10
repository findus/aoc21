import scala.io.Source

object day10 extends App{
  val filename = "src/main/resources/day10"
  val file = Source.fromFile(filename)
  val lines = file.getLines().toList

  case class Chunk(chunks: List[Chunk], str: String)

  var openClose = List(
    ('[' -> ']'),
    ('{' -> '}'),
    ('<' -> '>'),
    ('(' -> ')')
  ).toMap

  var closeOpen = List(
    (']' -> '['),
    ('}' -> '{'),
    ('>' -> '<'),
    (')' -> '(')
  ).toMap

  var score = List(
    (')' -> 3),
    (']' -> 57),
    ('}' -> 1197),
    ('>' -> 25137)
  ).toMap

  var part2Score = List(
    (')' -> 1),
    (']' -> 2),
    ('}' -> 3),
    ('>' -> 4)
  ).toMap

  def isOpening(char: Char): Boolean = {
    openClose.isDefinedAt(char)
  }

  object State extends Enumeration {
    type State = Value
    val OK,ERR,UNFINISHED = Value
  }


  val result = lines.zipWithIndex.map(line => {
    val stack = scala.collection.mutable.Stack.empty[Char]
    val res = line._1.foldLeft((stack,' ', State.OK))((prev,char) => {
      if (prev._3 == State.ERR) {
        prev
      } else {
        if (isOpening(char)) {
          prev._1.push(char)
          (prev._1, char, State.OK)
        } else {
          val lastOpening = prev._1.pop()
          if (openClose(lastOpening) != char) {
            (prev._1, char, State.ERR)
          } else {
            (prev._1, char, State.OK)
          }
        }
      }
    })
    if (res._1.nonEmpty && res._3 == State.OK) { (res._1,res._2, State.UNFINISHED) } else { res }
    })

  //Part 1
  println(result
    .filter(e => e._3 == State.ERR)
    .map(e => e._2)
    .groupBy(e => e)
    .view.mapValues(_.size)
    .map(e => score(e._1) * e._2)
    .sum
  )

  //Part 2
  val res = result
    .filter(e => e._3 == State.UNFINISHED)
    .map(e => e._1)
    .map(f =>  f.map(entry => openClose(entry)))
    .map(line => line.foldLeft(0L)((prev,data) => (prev * 5) + part2Score(data)) )
    .sorted

  println(res)
  println(res.length)
  println(res(res.length / 2))


}
