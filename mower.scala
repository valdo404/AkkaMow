import scala.Enumeration
import scala.collection.immutable.IndexedSeq
import scala.collection.Iterator

/** Mower service is a service whose goal is to move mowers */
object MowerService {
  /**
   * service execution
   * @param args does not anything
   */
  def main(args: Array[String]) {
    println("Beginning mowing")

    MowerProgrammingService.execute(io.Source.stdin.getLines)

    println("Mowed terminated")
  }

  /**
   * Mower programming service is a singleton whose goal is parsing flat files
   * it also build lawn and mowers. After that it takes mowers and make them move
   */
  object MowerProgrammingService {
    /**
     * Execute the service on several lines
     *
     * @param lines the configuration lines
     */
    def execute(lines: Iterator[String]) {
      val lawn = parseLawn(lines.next())

      while(lines.hasNext) {
        val mower = parseMower(lawn, lines.next())
        val instructions = parseMowerInstructions(lines.next())

        for(instruction <- instructions) mower.operate(instruction)

        mower.tell()
      }
    }

    /**
     * Builds a lawn from a lawn configuration line
     * @param line the lawn line
     * @return a Lawn
     */
    def parseLawn(line: String): Lawn = {
      val values = line.split(' ')

      new Lawn((values(0).toInt, values(1).toInt))
    }

    /**
     * Builds a mower from a mower configuration line
     * @param line the mower description line
     * @return a Mower
     */
    def parseMower(lawn: Lawn, line: String): Mower = {
      val values = line.split(' ')
      val orientation = Orientation.withName(values(2).toString)

      new Mower(lawn, values(0).toInt, values(1).toInt, orientation)
    }

    /**
     * Builds mower instructions from a mower instructions line
     * @param line the mower instructions line
     * @return list of mower Operation
     */
    def parseMowerInstructions(line: String): IndexedSeq[Operation.Value] = {
      for(current<-line) yield Operation.withName(current.toString)
    }
  }

  /**
   * Mower entity which is able to interpret orders as well
   *
   * @param lawn the associated lawn
   * @param x initial x
   * @param y initial y
   * @param orientation initial orientation
   * @throws IllegalArgumentException if x and y does not comply with lawn size
   */
  class Mower(val lawn: Lawn, var x: Int, var y:Int, var orientation: Orientation.Value) {

    if(!lawn.in((x,y)))
      throw new IllegalArgumentException("Not in lawn")

    /**
     * Operate given instruction and updates internal state
     *
     * @param instruction operation spec
     */
    def operate(instruction: Operation.Value) {
      var position = (x,y)

      if(instruction == Operation.Left || instruction == Operation.Right)
        orientation = rotate(instruction)
      else if(instruction == Operation.Forward)
        position = forward()
        if(!lawn.in(position)) // do not raise anything
          return
      
      x = position._1
      y = position._2
    }

    /**
     * Does a rotation
     *
     * @param instruction the rotating spec
     * @return new Orientation
     */
    def rotate(instruction: Operation.Value): Orientation.Value = {
      val rotatingSpec = Map(
        (Orientation.North, Orientation.West),
        (Orientation.West, Orientation.South),
        (Orientation.South, Orientation.East),
        (Orientation.East, Orientation.North)
      )

      val reverseSpec = rotatingSpec map {_.swap}

      instruction match {
        case Operation.Left => rotatingSpec(orientation)
        case Operation.Right => reverseSpec(orientation)
      }
    }

    /**
     * Does a forwarding operation
     * @return new position
     */
    def forward(): (Int, Int) = {
      orientation match {
        case Orientation.North => (x, y+1)
        case Orientation.East => (x+1, y)
        case Orientation.South => (x, y-1)
        case Orientation.West => (x-1, y)
      }
    }

    /**
     * Tell the new state, as described in associated specification
     */
    def tell() {
      println("I have coordinates "+ x + ',' + y + " and orientation " + orientation)
    }
  }

  /**
   * Lawn entity.
   * @param size lawn size
   */
  class Lawn(val size: (Int,Int)) {
    val origin=(0,0)

    def in(position: (Int, Int)): Boolean = {
      position._1 <= size._1 && position._2 <= size._2
    }
  }

  /**
   * Orientation spec
   */
  object Orientation extends Enumeration {
    val North = Value("N")
    val East = Value("E")
    val West = Value("W")
    val South = Value("S")
  }

  /**
   * Operation spec
   */
  object Operation extends Enumeration {
    val Left = Value("G")
    val Right = Value("D")
    val Forward = Value("A")
  }
}
