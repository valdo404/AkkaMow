package mower


/**
 * Mower entity which is able to interpret orders as well
 *
 * @param lawn the associated lawn
 * @param x initial x
 * @param y initial y
 * @param orientation initial orientation
 * @throws IllegalArgumentException if x and y does not comply with lawn size
 */
case class Mower(lawn: Lawn, var x: Int, var y:Int, var orientation: Orientation) {
  if(!lawn.in((x,y)))
    throw new IllegalArgumentException("Not in lawn")

  /**
   * Operate given instruction and updates internal state
   *
   * @param instruction operation spec
   */
  def operate(instruction: Operation) {
    var position = (x,y)

    if(instruction == ToLeft || instruction == ToRight)
      orientation = rotate(instruction)
    else if(instruction == Forward)
      position = forward()

    if(!lawn.in(position))
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
  def rotate(instruction: Operation): Orientation = {
    val rotatingSpec = Map[Orientation, Orientation] ((North, West), (West, South), (South, East), (East, North))

    instruction match {
      case ToLeft => rotatingSpec.get(orientation).get
      case ToRight => (rotatingSpec map {_.swap}).get (orientation).get
      case _ => orientation
    }
  }

  /**
   * Does a forwarding operation
   * @return new position
   */
  def forward(): (Int, Int) = {
    orientation match {
      case North => (x, y+1)
      case East => (x+1, y)
      case South => (x, y-1)
      case West => (x-1, y)
    }
  }

  /**
   * Tell the new state, as described in associated specification
   */
  def tell() {
    println("I have coordinates "+ x + ',' + y + " and orientation " + orientation)
  }

  /**
   * Orientation spec
   */

}

/**
 * Lawn entity.
 * @param size lawn size
 */
case class Lawn(size: (Int,Int)) {
  val origin=(0,0)

  def in(position: (Int, Int)): Boolean = {
    position._1 <= size._1 && position._2 <= size._2
  }
}

class Orientation
case object North extends Orientation
case object South extends Orientation
case object East extends Orientation
case object West extends Orientation

class Operation
trait MoveOperation
case object ToLeft extends Operation
case object ToRight extends Operation
case object Forward extends Operation with MoveOperation
