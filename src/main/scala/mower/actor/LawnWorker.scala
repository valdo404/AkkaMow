package mower.actor

import akka.actor.{Actor, ActorSystem, Props}
import akka.cluster.routing.{ClusterRouterPool, ClusterRouterPoolSettings}
import akka.event.Logging
import akka.kernel.Bootable
import akka.routing.ConsistentHashingPool
import com.typesafe.config.Config
import mower.{Lawn, Mower, Operation}

class LawnWorker extends Actor {
  def receive = {
    case LaunchMower(number, mower, operations) =>
      val mower = context.actorOf(Props(new MowerActor(number, mower)), number.toString)
      for(operation <- operations)
        mower ! (operation, self)
}

class LawnActor(val lawn: Lawn) extends Actor {
  val log = Logging(context.system, this)

  val lawnRouter = context.actorOf(
    ClusterRouterPool(ConsistentHashingPool(10), ClusterRouterPoolSettings(
      totalInstances = 10, maxInstancesPerNode = 3, allowLocalRoutees = true, useRole = None)).props(Props[LawnWorker]),
    name = "router")

  override def preStart() = {
    log.info("Lawn is starting")
  }

  def receive = {
    case launch: LaunchMower =>
      lawnRouter ! launch
    case Tell =>
      context.actorSelection("*/*").forward(Tell)
  }
}

class MowerKernel(config: Config) extends Bootable {
  val system = ActorSystem("mowers", config)

  def startup() = {}

  def shutdown() = {
    system.shutdown()
  }
}

case class Tell()
case class LaunchMower(number: Int, mower: Mower, operations: Seq[Operation])

