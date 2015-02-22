package mower.actor

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.{ActorSystem, Props, _}
import mower.{Lawn, MowerParser}

object MowerActorProgrammer {
  def execute(lines: Iterator[String]) = {
    val lawn = MowerParser.lawn(lines.next()) fold (lawn => lawn, error => throw new Exception(error))

    Thread.sleep(10000)

    val counter = new AtomicInteger
    val system = ActorSystem("mowers")
    val mowerService = system.actorOf(Props(new LawnActor(lawn)), "lawn")

    while(lines.hasNext) {
      val mower = MowerParser.mower(lawn, lines.next()) fold (
        possible => possible, error => throw new Exception(error))
      val instructions = MowerParser.instructions(lines.next())

      mowerService ! Tell
      mowerService ! LaunchMower(counter.addAndGet(1), mower,
        for(instruction <- instructions) yield instruction fold(instruction => instruction, error => throw new Exception(error)))
    }

    mowerService ! Tell
  }
}
