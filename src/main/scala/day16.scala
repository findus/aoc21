import day10.State.Value

object PacketType extends Enumeration  {
  type State = Value
  val Literal, Operator = Value
}

object day16 extends App {
  //println(startParse("D2FE28"))
  //rintln(startParse("38006F45291200"))
  println(startParse("EE00D40C823060"))

  def startParse(string:String) = {
    val binaryInput = BigInt(string,16).toString(2)
    val padding = binaryInput.length % 4
    val paddedBinaryInput = (1 to padding).foldLeft(binaryInput)((prev,data) => "0" + prev)
    parse(paddedBinaryInput)
  }

  def parse(input: String): List[(Int,Int,(Int,Int))] = {

    val (v,t) = versionAndType(input)
    t match {
      case 4 => {
        val nr = numberFromLiteral(input)
        List((v,4,nr))
      }
      case operatorVersion@_ => {
        input(6).asDigit match {
          case 0 => return handle0Operation(input).::(v,operatorVersion,(0,0))
          case 1 => return handle1Operation(input)
        }
        List.empty
      }
    }
  }

  def versionAndType(string: String) = {
    val version = Integer.parseInt(string.substring(0,3),2)
    val packetType = Integer.parseInt(string.substring(3,6),2)
    (version,packetType)
  }

  def getIndizes(string: String) = {
    val data = string.substring(6, string.length - 1)
    val test = data.sliding(5,5).toList
  }

  def numberFromLiteral(string: String) = {
    val data = string.substring(6, string.length - 1)
    val test = data.sliding(5,5).foldLeft((List.empty[String], true))((prev,data) =>  {
      if (prev._2) {
        if (data.startsWith("0")) {
          (prev._1.::(data), false)
        } else {
          (prev._1.::(data), true)
        }
      } else {
        (prev._1, false)
      }
    })
    val m = test._1.reverse.map(e => e.drop(1)).mkString("")
    (Integer.parseInt(m,2),  7 + m.length)
  }

  def handle1Operation(input: String) = {
    val nrOfPackets = input.substring(7, 18)
    val nrOfPackets2 = Integer.parseInt(nrOfPackets,2)
    val e  = (0 to (nrOfPackets2 - 1)).foldLeft(List.empty[(Int,Int,(Int,Int))])((prev,data) => {
      prev.++(parse(input.substring(18 + (data * 11),29 + (data * 11) )))
    })
    e
  }

  def handle0Operation(paddedBinaryInput: String) = {
    val data = paddedBinaryInput.substring(7, 22)
    val number = Integer.parseInt(data,2)
    val endOfSubPackets = 22 + number + 1

    var subPackets = List.empty[(Int,Int,(Int,Int))]
    var offset = 0
    while (offset + 22 < endOfSubPackets && endOfSubPackets < paddedBinaryInput.length && paddedBinaryInput.substring(22 + offset, endOfSubPackets).length >= 6) {
      val p = paddedBinaryInput.substring(22 + offset, endOfSubPackets)
      val result = parse(p)
      subPackets = subPackets.++(result)
      offset = offset + result.head._3._2
    }

    subPackets
  }


}
