
package mower

import org.scalatest._

class MowerSpecs extends FlatSpec with Matchers {

  "A mower" should "operate instructions" in {
    val lawn = new Lawn((5,5))
    val mower = new Mower(lawn, 1, 2, Orientation.North)

    mower.operate(Operation.Left)
    mower.operate(Operation.Forward)
    mower.operate(Operation.Left)
    mower.operate(Operation.Forward)
    mower.operate(Operation.Left)
    mower.operate(Operation.Forward)
    mower.operate(Operation.Left)
    mower.operate(Operation.Forward)
    mower.operate(Operation.Forward)

    mower.x should be (1)
    mower.y should be (3)
    mower.orientation should be (Orientation.North)
  }
}