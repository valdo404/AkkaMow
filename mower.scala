import scala.Enumeration
import scala.collection.immutable.IndexedSeq


object MowerService {
  def main(args: Array[String]) {
    println("Beginning parse")
    val lines = io.Source.stdin.getLines

    var lawn = parseSize(lines.next())

    while(lines.hasNext) {
      var mower = parseInitial(lawn, lines.next())
      var instructions = parseInstructions(lines.next())

      for(instruction <- instructions) mower.operate(instruction)
    }
    println("End parse")
  }

  def parseSize(line: String): Lawn = {
    val vals = line.split(' ')
    new Lawn((vals(0).toInt, vals(1).toInt))
  }

  def parseInitial(lawn: Lawn, line: String): Mower = {
    val vals = line.split(' ')
    val orientation = Orientation.withName(vals(2).toString)

    new Mower(lawn, vals(0).toInt,vals(1).toInt, orientation)
  }

  def parseInstructions(line: String): IndexedSeq[Operation.Value] = {
    for(cur<-line) yield Operation.withName(cur.toString)
  }


  class Mower(val lawn: Lawn, var x: Int, var y:Int, var orientation: Orientation.Value) {
    def operate(instruction: Operation.Value) {
      var position = (x,y)

      if(instruction == Operation.Left || instruction == Operation.Right)
        orientation = rotate(instruction)
      else if(instruction == Operation.Forward)
        position = forward()

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
