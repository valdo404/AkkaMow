
package mower

import org.scalatest._

class MowerSpecs extends FlatSpec with Matchers {

  "A mower" should "operate instructions" in {
    val lawn = new Lawn((5,5))
    val mower = new Mower(lawn, 1, 2, North)

    mower.operate(ToLeft)
    mower.operate(Forward)
    mower.operate(ToLeft)
    mower.operate(Forward)
    mower.operate(ToLeft)
    mower.operate(Forward)
    mower.operate(ToLeft)
    mower.operate(Forward)
    mower.operate(Forward)

    mower.x should be (1)
    mower.y should be (3)
    mower.orientation should be (North)
  }


  "A mower" should "go forward when required" in {
    val lawn = new Lawn((5,5))
    val mower = new Mower(lawn, 1, 1, North)

    mower.operate(Forward)

    mower.x should be (1)
    mower.y should be (2)

    mower.orientation should be (North)
  }

  "A lawn" should "be parsed correctly" in {
    MowerParser.parseLawn("a") should matchPattern{case Right("Bad length") => }
    MowerParser.parseLawn("a b") should matchPattern{case Right("Not an integer") => }
    MowerParser.parseLawn("1 3") should matchPattern{case Left(Lawn((1,3))) => }
  }

  "An instruction" should "be parsed correctly" in {
    MowerParser.parseMowerInstructions("a") should matchPattern{case Vector(Right("Unknown operation")) => }
    MowerParser.parseMowerInstructions("") should matchPattern{case Vector() => }
    MowerParser.parseMowerInstructions("LRF") should matchPattern{case Vector(Left(ToLeft), Left(ToRight), Left(Forward)) => }
  }

  "A mower" should "be parsed correctly" in {
    MowerParser.parseMower(Lawn((1, 1)), "a") should matchPattern{case Right("Bad length") => }
    MowerParser.parseMower(Lawn((1, 1)), "a b N") should matchPattern{case Right("Not an integer") => }
    MowerParser.parseMower(Lawn((3, 3)), "1 2 P") should matchPattern{case Right("Unknown orientation") => }
    MowerParser.parseMower(Lawn((1, 1)), "1 2 N") should matchPattern{case Right("Not in lawn") => }

    MowerParser.parseMower(Lawn((3, 3)), "1 2 N") should matchPattern{case Left(_) => }
  }
}