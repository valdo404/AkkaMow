
package mower.tests

import org.scalatest._
import mower._

class MowerSpecs extends FlatSpec with Matchers {

  "A mower" should "operate instructions" in {
    val lawn = new MowerService.Lawn((5,5))
    val mower = new MowerService.Mower(lawn, 1, 2, MowerService.Orientation.North)

    mower.operate(MowerService.Operation.Left)
    mower.operate(MowerService.Operation.Forward)
    mower.operate(MowerService.Operation.Left)
    mower.operate(MowerService.Operation.Forward)
    mower.operate(MowerService.Operation.Left)
    mower.operate(MowerService.Operation.Forward)
    mower.operate(MowerService.Operation.Left)
    mower.operate(MowerService.Operation.Forward)
    mower.operate(MowerService.Operation.Forward)

    mower.x should be (1)
    mower.y should be (3)
    mower.orientation should be (MowerService.Orientation.North)
  }



}