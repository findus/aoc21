import day13.folded2

import scala.collection.mutable
import scala.io.Source

case class Edge(position: (Int,Int), cost: Int)
case class CCave(position: (Int,Int), var distance: Int, risk: Int, neighbours: List[Edge], var previous: Option[CCave] = None, var visited: Boolean = false) extends Ordering[CCave] {
  override def compare(x: CCave, y: CCave): Int = x.distance.compareTo(y.distance)
}

object day15 extends App {
  val filename = "src/main/resources/day15"
  val file = Source.fromFile(filename)
  val lines = file.getLines().toList



  val height = lines.length
  val width = lines.head.length

  val caves = lines.zipWithIndex.flatMap(y => (y._1.toCharArray.zipWithIndex.map(c => CCave((c._2, y._2),10000, c._1.asDigit, List.empty))))

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

    //println(dijkstra(start, cavesWithNeighbours))
    println(dijkstra2(start, end, cavesWithNeighbours))
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

    val start = fivexcave2.head
    val end = fivexcave2.last

    println(dijkstra2(start, end ,fivexcave2))
  }

 // part1()
  part2()

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

  def printPath(stop: CCave, input: List[CCave]) = {
    var path = List(stop)
    var c = stop
    while (c.previous.isDefined) {
      c = c.previous.get
      path = path.::(c)
    }

    println("")
    println("----------------------------------")

    val maxY = input.map(e => e.position._1).max
    val maxX = input.map(e => e.position._2).max

    var lastY = 0
    for {
      y <- (0 to maxY)
      x <- (0 to maxX)
    } {
      if (lastY != y) {
        println("")
        lastY = y
      }
      val entry = input.find(c => c.position == (x,y))
      if (entry.isDefined && path.map(_.position).contains(entry.get.position)) {
        print("X")
      } else {
        print(" ")
      }
    }
  }

  def dijkstra2(start : CCave, to: CCave, input: List[CCave]): Int = {
    implicit val ord: Ordering[CCave] = (x: CCave, y: CCave) => y.distance.compareTo(x.distance)
    var settled = List.empty[CCave]
    val queue = mutable.PriorityQueue.empty[CCave]
    queue.addOne(start)
    start.distance = 0
    var running = true

    // Not visited
    var thread = new Thread(new Runnable {
      override def run(): Unit = while(running) {
        println(settled.length.toDouble / input.length.toDouble)
        Thread.sleep(2000)
      }
    })
    thread.start()

    val maxX = input.map(e=>e.position).maxBy(f => f._1)._1
    val maxY = input.map(e=>e.position).maxBy(f => f._2)._2

    while (settled.size != input.size) {

      if (queue.isEmpty) {
        running = false
        return 0
      }

      val entry = queue.dequeue()

      if (entry == to) {
        running = false
        return entry.distance
      }

      if (!settled.contains(entry)) {
        entry.visited = true
        settled = settled.::(entry)
        val nextShortestCave = getNeightbours(entry,maxX,maxY).foldLeft(List.empty[CCave])((prev,data) => prev.::(input.find(p => p.position == data).get)).sortBy(_.risk)
        for {neighbour <- nextShortestCave} {
          if (!settled.contains(neighbour)) {
            val dis = neighbour.risk
            val newdis = entry.distance + dis
            if (newdis < neighbour.distance) {
              neighbour.distance = newdis
              neighbour.previous = Some(entry)
            }
            queue.addOne(neighbour)
          }
        }
      }

    }

    running =false

    return input.last.distance
  }


  def distance_update(nextShortestCave: CCave, neighbour: Edge, caves: List[CCave]) = {
    val alt = nextShortestCave.distance + neighbour.cost
    val cave = caves.find(c => c.position == neighbour.position).get
    if (alt < cave.distance ) {
      cave.distance = alt
      cave.previous = Some(nextShortestCave)
      printPath(cave,caves)
    }
  }

  def dijkstra(start: CCave, Q: List[CCave]): Int = {
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
    return Q.last.distance
  }


}
