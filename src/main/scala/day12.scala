import scala.io.Source

case class Cave(name: String,big: Boolean, val visitCount: Int, var edges: List[String])

object day12 extends App {

  var caveSystem = List.empty[Cave]
  var paths = List.empty[List[Cave]]

  def addConnection(connection: (String,String)) = {
    val cave1 = caveSystem.find(e => e.name == connection._1).get
    val cave2 = caveSystem.find(e => e.name == connection._2).get
    cave1.edges = cave1.edges.::(cave2.name)
    cave2.edges = cave2.edges.::(cave1.name)
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
        list = list.++(dfs(nextHop,goal, path.::(nextHop.name), system.updated(caveSystem.indexOf(cave),thisCave)))
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
    caveSystem = caves
    val connections = lines.map(line => (line.split("-")(0),line.split("-")(1)))
    connections.foreach(connection => addConnection(connection))

    val start = caveSystem.find(cave => cave.name == "start").get
    val end = caveSystem.find(cave => cave.name == "end").get

    val res = dfs(start,end.name,List.empty, caveSystem).filter(e => e != null)
    println(res.length)
  }

  def part2() = {
    //Part2
    caveSystem = caves
    val connections = lines.map(line => (line.split("-")(0),line.split("-")(1)))
    connections.foreach(connection => addConnection(connection))

    val start = caveSystem.find(cave => cave.name == "start").get
    val end = caveSystem.find(cave => cave.name == "end").get

    val res = dfs(start,end.name,List.empty, caveSystem).filter(e => e != null)
    println(paths.length)
  }

  part1()
  paths = List.empty
  part2()




}
