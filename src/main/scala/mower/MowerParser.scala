package mower
import scala.util.{Left => L, Right => E}
import scala.util.control.Exception._

/** Mower parser is in charge of mower parsing **/
object MowerParser {
  val BadLength = E("Bad length")
  val NotInLawn = E("Not in lawn")
  val NoOrientation = E("Unknown orientation")
  val NoOp = E("Unknown operation")
  val NotAnInteger = E("Not an integer")

  def lawn(line: String): Either[Lawn, String] = {
    val values = line.split(' ')

    if (values.length < 2) BadLength
    else coords(values(0), values(1)) fold (
        c => L(Lawn((c._1, c._2))),
        e => E(e))
  }

  def coords(x: String,y: String): Either[(Int, Int), String] = {
    parseInt(x).fold (x => parseInt(y) fold (y => L((x, y)), e => E(e)), e => E(e))
  }

  def mower(lawn: Lawn, line: String): Either[Mower, String] = {
    val values = line.split(' ')
    if (values.length < 3) BadLength
    else orientation(values(2).toString) fold (
      orientation =>
        coords(values(0), values(1)) fold(
          coords => if (lawn.in(coords)) L(new Mower(lawn, coords, orientation)) else NotInLawn,
          error => E(error)), error => E(error)
    )
  }

  def orientation(value: String): Either[Orientation, String] = value match {
    case "E" => L(East) case "W" => L(West) case "N" => L(North) case "S" => L(South) case _ => NoOrientation
  }

  def instructions(line: String): Seq[Either[Operation, String]] = {
    for (current <- line) yield operation(current.toString)
  }

  def operation(value: String): Either[Operation, String] = value match {
    case "L" => L(ToLeft) case "R" => L(ToRight) case "F" => L(Forward) case _ => NoOp
  }

  def parseInt(x: String): Either[Int, String] = {
    (catching(classOf[NumberFormatException]) either {x.toInt}).swap.right.flatMap(x => NotAnInteger)
  }
}
