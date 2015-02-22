package mower.actor

import akka.actor._
import akka.event.Logging
import mower._

import scala.language.postfixOps


class MowerActor(val number: Int, val mower: Mower) extends Actor {
  val log = Logging(context.system, this)

  def receive = {
    case ToLeft => mower(ToLeft)
    case ToRight => mower(ToRight)
    case Forward => mower(Forward)
    case Tell => log.info("Mower {} is {} and {}", number, mower.position, mower.orientation)
  }
}




