import scala.collection.mutable
import scala.io.Source

case class Edge(position: (Int,Int), cost: Int)
case class CCave(position: (Int,Int), var distance: Int, risk: Int, neighbours: List[Edge], var previous: Option[CCave] = None)

object day15 extends App {
  val filename = "src/main/resources/day15"
  val file = Source.fromFile(filename)
  val lines = file.getLines().toList



  val height = lines.length
  val width = lines.head.length

  val caves = lines.zipWithIndex.flatMap(y => (y._1.toCharArray.zipWithIndex.map(c => CCave((c._2, y._2),-1, c._1.asDigit, List.empty))))

  def part1(): Unit = {

    def inBounds(coords: (Int,Int)): Boolean = {
      coords._1 < width && coords._2 < height && coords._1 >= 0 && coords._2 >= 0
    }

    val cavesWithNeighbours = caves.map(cave => {
      val neighbours = Seq(
        (cave.position._1,cave.position._2 - 1),
        (cave.position._1, cave.position._2 + 1),
        (cave.position._1 - 1, cave.position._2 ),
        (cave.position._1 + 1, cave.position._2 ),
      ).map(neigbour => { Some(neigbour).filter(inBounds) })
        .filter(n => n.isDefined)
        .map(n => n.get)
        .map(n => Edge(n, caves.find(c => c.position == n).get.risk))
        .toList

      CCave(cave.position,10000, cave.risk, neighbours)
    })

    val end = cavesWithNeighbours.last
    val start = cavesWithNeighbours.head


    dijkstra(start, cavesWithNeighbours)
    shortestPath(end);
  }

  def part2() = {

    val fivexcave = caves.sliding(10).flatMap{ case caveRow =>
      (1 to 4).foldLeft(caveRow)((prev,data) => {
        val newPosX = (data * width)
        prev.++(caveRow.map(c => {
          val c1 = CCave((c.position._1 + newPosX, c.position._2), c.distance, if ((c.risk + data) % 9== 0) { 9 } else {  (c.risk + data) % 9 }, c.neighbours, None)
          c1
        }))
      })
    }.toList.distinct.sortBy(_.position._2)

    val fivexcave2 = (1 to 4).foldLeft(fivexcave)((prev,data) => {
      val newPosY = (data * height)
      val m = fivexcave.map(c => {
        val c1 = CCave((c.position._1, c.position._2 + newPosY), c.distance, if ((c.risk + data) % 9== 0) { 9 } else {  (c.risk + data) % 9 }, c.neighbours, None)
        c1
      })
      prev.++(m)
    }).distinct.sortBy(_.position._2).toList

    val nheight = fivexcave2.maxBy(_.position._2).position._2
    val nwidth = fivexcave2.maxBy(_.position._1).position._1


    val start = fivexcave2.head
    val end = fivexcave2.last


    dijkstra(start, fivexcave2)
    shortestPath(end);
  }

  part1()
  //part2()

  def getNeightbours(cave: CCave, width: Int, height: Int): List[(Int,Int)] = {

    def inBounds(coords: (Int,Int)): Boolean = {
      coords._1 <= width && coords._2 <= height && coords._1 >= 0 && coords._2 >= 0
    }

    Seq(
      (cave.position._1,cave.position._2 - 1),
      (cave.position._1, cave.position._2 + 1),
      (cave.position._1 - 1, cave.position._2 ),
      (cave.position._1 + 1, cave.position._2 ),
    ).map(neigbour => { Some(neigbour).filter(inBounds) })
      .filter(n => n.isDefined)
      .map(n => n.get)
      .toList
  }


  def distance_update(nextShortestCave: CCave, neighbour: Edge, caves: List[CCave]) = {
    val alt = nextShortestCave.distance + neighbour.cost
    val cave = caves.find(c => c.position == neighbour.position).get
    if (alt < cave.distance ) {
      cave.distance = alt
      cave.previous = Some(nextShortestCave)
    }
  }

  def dijkstra(start: CCave, Q: List[CCave]): Unit = {
    val predecessor = None
    start.distance = 0
    var q = Q
    while (q.nonEmpty) {
      val nextShortestCave = q.minBy(_.distance)
      q = q.filter(c => c != nextShortestCave).toList
      for { neighbour <- nextShortestCave.neighbours } {
        if ( q.exists(c => c.position == neighbour.position)) {
          distance_update(nextShortestCave, neighbour, Q)
        }
      }
    }
    return predecessor
  }

  def shortestPath(end: CCave) = {
    var c: Option[CCave] = Some(end);
    var risk = 0
    while( c.isDefined) {
      if (c.get.previous.isDefined) {
        risk = risk + c.get.risk
      }
      c = c.get.previous
    }
    println(risk)
  }


}
