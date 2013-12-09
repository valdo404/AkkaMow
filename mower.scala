import scala.Enumeration
import scala.collection.immutable.IndexedSeq
import scala.collection.Iterator

object MowerService {
  def main(args: Array[String]) {
    println("Beginning mowing")

    ParsingService.execute(io.Source.stdin.getLines)

    println("End parse")
  }

  object ParsingService {
    def execute(lines: Iterator[String]) {
      val lawn = parseLawn(lines.next())

      while(lines.hasNext) {
        val mower = parseMower(lawn, lines.next())
        val instructions = parseMowerInstructions(lines.next())

        for(instruction <- instructions) mower.operate(instruction)

        mower.tell()
      }
    }

    def parseLawn(line: String): Lawn = {
      val values = line.split(' ')

      new Lawn((values(0).toInt, values(1).toInt))
    }

    def parseMower(lawn: Lawn, line: String): Mower = {
      val values = line.split(' ')
      val orientation = Orientation.withName(values(2).toString)

      new Mower(lawn, values(0).toInt, values(1).toInt, orientation)
    }

    def parseMowerInstructions(line: String): IndexedSeq[Operation.Value] = {
      for(current<-line) yield Operation.withName(current.toString)
    }
  }

  class Mower(val lawn: Lawn, var x: Int, var y:Int, var orientation: Orientation.Value) {
    def operate(instruction: Operation.Value) {
      var position = (x,y)

      if(instruction == Operation.Left || instruction == Operation.Right)
        orientation = rotate(instruction)
      else if(instruction == Operation.Forward)
        position = forward()

      if(!lawn.in(position))
        return
      
      x = position._1
      y = position._2
    }

    def rotate(instruction: Operation.Value): Orientation.Value = {
      val rotatingSpec = Map(
        (Orientation.North, Orientation.West),
        (Orientation.East, Orientation.North),
        (Orientation.South, Orientation.East),
        (Orientation.West, Orientation.South))

      val reverseSpec = rotatingSpec map {_.swap}
      
      instruction match {
        case Operation.Left => rotatingSpec(orientation)
        case Operation.Right => reverseSpec(orientation)
      }
    }

    def forward(): (Int, Int) = {
      orientation match {
        case Orientation.North => (x, y+1)
        case Orientation.East => (x+1, y)
        case Orientation.South => (x, y-1)
        case Orientation.West => (x-1, y)
      }
    }

    def tell() {
      println("I have coordinates "+ x + ',' + y + " and orientation " + orientation)
    }
  }

  class Lawn(val size: (Int,Int)) {
    val origin=(0,0)

    def in(position: (Int, Int)): Boolean = {
      position._1 <= size._1 && position._2 <= size._2
    }
  }

  object Orientation extends Enumeration {
    val North = Value("N")
    val East = Value("E")
    val West = Value("W")
    val South = Value("S")
  }

  object Operation extends Enumeration {
    val Left = Value("G")
    val Right = Value("D")
    val Forward = Value("A")
  }
}
