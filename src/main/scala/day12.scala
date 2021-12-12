import scala.io.Source

case class Cave(name: String,big: Boolean, val visitCount: Int, val edges: List[String])

object day12 extends App {

  var paths = List.empty[List[Cave]]

  def addConnection(list: List[Cave], connection: (String,String)) = {
    val cave1 = list.find(e => e.name == connection._1).get
    val cave2 = list.find(e => e.name == connection._2).get
    val cave1_u = cave1.copy(edges = cave1.edges.::(cave2.name))
    val cave2_u = cave2.copy(edges = cave2.edges.::(cave1.name))
    val l = list.updated(list.indexOf(cave1), cave1_u).updated(list.indexOf(cave2),cave2_u)
    l
  }

  def dfs(cave: Cave, goal: String, path: List[String], system: List[Cave]): List[String] = {
    val neighbours = scala.collection.mutable.Stack
      .from(system.filter(c => cave.edges.contains(c.name)))
      .filter(c => c.visitCount > 0)

    var thisCave = cave
    if (!cave.big) {
      thisCave = cave.copy(visitCount = (cave.visitCount - 1))
    }

    if (cave.name == goal) {
      List(path.reverse.mkString("->"))
    } else {
      var list = List.empty[String]
      while (neighbours.nonEmpty) {
        val nextHop = neighbours.pop()
        list = list.++(dfs(nextHop,goal, path.::(nextHop.name), system.updated(system.indexOf(cave),thisCave)))
      }
      list
    }
  }

  val filename = "src/main/resources/day12"
  val file = Source.fromFile(filename)
  val lines = file.getLines().toList
  val caves = lines.flatMap(line => line.split("-")).sorted.distinct.map(cave => Cave(cave, cave.toUpperCase == cave && cave != "start" && cave != "end", 1, List.empty))

  //Part1
  def part1() = {
    val connections = lines.map(line => (line.split("-")(0),line.split("-")(1)))
    val system = connections.foldLeft(caves)((prev,data) => addConnection(prev, data))

    val start = system.find(cave => cave.name == "start").get
    val end = system.find(cave => cave.name == "end").get

    val res = dfs(start,end.name,List.empty, system).filter(e => e != null)
    println(res.length)
  }

  def part2() = {

  }

  part1()
  paths = List.empty
  part2()




}
