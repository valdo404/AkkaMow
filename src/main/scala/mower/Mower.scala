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

case class Mower(lawn: Lawn, var position: (Int, Int), var orientation: Orientation) {
  val rotate = Map[Orientation, Orientation] ((North, West), (West, South), (South, East), (East, North))

  def apply(instruction: Operation) {
    newState(operate(instruction))
  }

  def x = position._1
  def y = position._2

  def newState(state: ((Int, Int), Orientation)): Unit = {
    orientation = state._2
    position = state._1
  }

  def operate(instruction: Operation): ((Int, Int), Orientation) = instruction match {
    case ToLeft => (position, rotate(instruction))
    case ToRight => (position, rotate(instruction))
    case Forward => (forward(position, orientation), rotate(instruction))
  }

  def rotate(instruction: Operation): Orientation = instruction match {
    case ToLeft => rotate.get(orientation).get
    case ToRight => (rotate map {_.swap}).get (orientation).get
    case _ => orientation
  }

  def forward(position: (Int, Int), direction: Orientation): (Int, Int) = {
    val computed = direction match {
      case North => (x, y+1) case East => (x+1, y) case South => (x, y-1) case West => (x-1, y)
    }

    if(!lawn.in(computed))
      position
    else
      computed
  }

  def tell() {
    println("I have coordinates "+ position + " and orientation " + orientation)
  }
}
