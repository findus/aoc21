import scala.io.Source

case class Edge(position: (Int,Int), cost: Int)
case class CCave(position: (Int,Int), var distance: Int, risk: Int, neighbours: List[Edge], var previous: Option[CCave] = None)

object day15 extends App {
  val filename = "src/main/resources/day15"
  val file = Source.fromFile(filename)
  val lines = file.getLines().toList
  val height = lines.length
  val width = lines.head.length

  def inBounds(coords: (Int,Int)): Boolean = {
    coords._1 < width && coords._2 < height && coords._1 >= 0 && coords._2 >= 0
  }

  val caves = lines.zipWithIndex.flatMap(y => (y._1.toCharArray.zipWithIndex.map(c => CCave((c._2, y._2),-1, c._1.asDigit, List.empty))))
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

  def distance_update(nextShortestCave: CCave, neighbour: Edge) = {
    val alt = nextShortestCave.distance + neighbour.cost
    val cave = cavesWithNeighbours.find(c => c.position == neighbour.position).get
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
          distance_update(nextShortestCave, neighbour)
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
