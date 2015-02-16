package mower


class Orientation
case object North extends Orientation
case object South extends Orientation
case object East extends Orientation
case object West extends Orientation

class Operation
case object ToLeft extends Operation
case object ToRight extends Operation
case object Forward extends Operation

case class Lawn(size: (Int,Int)) {
  def in(position: (Int, Int)): Boolean = position._1 <= size._1 && position._2 <= size._2
}

class Mower(val lawn: Lawn, var x: Int, var y: Int, var orientation: Orientation) {
  val rotate = Map[Orientation, Orientation] ((North, West), (West, South), (South, East), (East, North))

  def apply(instruction: Operation) {
    newState(operate(instruction))
  }

  def newState(state: ((Int, Int), Orientation)): Unit = {
    orientation = state._2
    x = state._1._1
    y = state._1._2
  }

  def operate(instruction: Operation): ((Int, Int), Orientation) = instruction match {
    case ToLeft => ((x, y), rotate(instruction))
    case ToRight => ((x, y), rotate(instruction))
    case Forward => (forward(x, y, orientation), rotate(instruction))
  }

  def rotate(instruction: Operation): Orientation = instruction match {
    case ToLeft => rotate.get(orientation).get
    case ToRight => (rotate map {_.swap}).get (orientation).get
    case _ => orientation
  }

  def forward(x: Int, y: Int, direction: Orientation): (Int, Int) = {
    val position = direction match {
      case North => (x, y+1) case East => (x+1, y) case South => (x, y-1) case West => (x-1, y)
    }

    if(!lawn.in(position))
      (x, y)
    else
      position
  }

  def tell() {
    println("I have coordinates "+ x + ',' + y + " and orientation " + orientation)
  }
}
