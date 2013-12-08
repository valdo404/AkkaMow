import scala.Enumeration
import scala.collection.immutable.IndexedSeq


object MowerService {
  def main(args: Array[String]) {
    println("Beginning parse")
    val lines = io.Source.stdin.getLines
    val lawn = parseSize(lines.next())

    while(lines.hasNext) {
      val mower = parseInitial(lawn, lines.next())
      val instructions = parseInstructions(lines.next())

      for(instruction <- instructions) mower.operate(instruction)

      mower.tell()
    }
    println("End parse")
  }

  def parseSize(line: String): Lawn = {
    val values = line.split(' ')
    new Lawn((values(0).toInt, values(1).toInt))
  }

  def parseInitial(lawn: Lawn, line: String): Mower = {
    val values = line.split(' ')
    val orientation = Orientation.withName(values(2).toString)

    new Mower(lawn, values(0).toInt, values(1).toInt, orientation)
  }

  def parseInstructions(line: String): IndexedSeq[Operation.Value] = {
    for(current<-line) yield Operation.withName(current.toString)
  }


  class Mower(val lawn: Lawn, var x: Int, var y:Int, var orientation: Orientation.Value) {
    def operate(instruction: Operation.Value) {
      var position = (x,y)

      if(instruction == Operation.Left || instruction == Operation.Right)
        orientation = rotate(instruction)
      else if(instruction == Operation.Forward)
        position = forward()

      if(position._1 > lawn.size._1 || position._2 > lawn.size._2)
        return
      
      x = position._1
      y = position._2
    }

    def rotate(instruction: Operation.Value): Orientation.Value = {
       instruction match {
        case Operation.Left => orientation match {
          case Orientation.North => Orientation.West
          case Orientation.East => Orientation.North
          case Orientation.South => Orientation.East
          case Orientation.West => Orientation.South
        }
        case Operation.Right => orientation match {
          case Orientation.West => Orientation.North
          case Orientation.North => Orientation.East
          case Orientation.East => Orientation.South
          case Orientation.South => Orientation.West
        }
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
