
package mower

import org.scalatest._

class MowerSpecs extends FlatSpec with Matchers {

  "A mower" should "operate instructions" in {
    val lawn = Lawn((5,5))
    val mower = new Mower(lawn, (1, 2), North)

    mower(ToLeft)
    mower(Forward)
    mower(ToLeft)
    mower(Forward)
    mower(ToLeft)
    mower(Forward)
    mower(ToLeft)
    mower(Forward)
    mower(Forward)

    mower.x should be (1)
    mower.y should be (3)
    mower.orientation should be (North)
  }


  "A mower" should "go forward when required" in {
    val lawn = new Lawn((5,5))
    val mower = new Mower(lawn, (1, 1), North)

    mower(Forward)

    mower.x should be (1)
    mower.y should be (2)

    mower.orientation should be (North)
  }

  "A lawn" should "be parsed correctly" in {
    MowerParser.lawn("a") should matchPattern{case Right("Bad length") => }
    MowerParser.lawn("a b") should matchPattern{case Right("Not an integer") => }
    MowerParser.lawn("1 3") should matchPattern{case Left(Lawn((1,3))) => }
  }

  "An instruction" should "be parsed correctly" in {
    MowerParser.instructions("a") should matchPattern{case Vector(Right("Unknown operation")) => }
    MowerParser.instructions("") should matchPattern{case Vector() => }
    MowerParser.instructions("LRF") should matchPattern{case Vector(Left(ToLeft), Left(ToRight), Left(Forward)) => }
  }

  "A mower" should "be parsed correctly" in {
    MowerParser.mower(Lawn((1, 1)), "a") should matchPattern{case Right("Bad length") => }
    MowerParser.mower(Lawn((1, 1)), "a b N") should matchPattern{case Right("Not an integer") => }
    MowerParser.mower(Lawn((3, 3)), "1 2 P") should matchPattern{case Right("Unknown orientation") => }
    MowerParser.mower(Lawn((1, 1)), "1 2 N") should matchPattern{case Right("Not in lawn") => }

    MowerParser.mower(Lawn((3, 3)), "1 2 N") should matchPattern{case Left(_) => }
  }

  "A list of mower" should "interpret line orders" in {
    MowerProgrammer.execute(Iterator("3 3", "1 2 N", "LRF", "2 3 E", "LRRF"))
  }

}