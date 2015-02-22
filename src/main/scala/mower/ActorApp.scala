package mower

import mower.actor.MowerActorProgrammer

import scala.collection.Iterator

object ActorApp extends App {
  MowerActorProgrammer.execute(Iterator(
    "3 3",
    "1 2 N",
    "LRF",
    "2 3 E",
    "LRRF",
    "1 2 N",
    "LRF",
    "2 3 E",
    "LRRF",
    "1 2 N",
    "LRF",
    "2 3 E",
    "LRRF",
    "1 2 N",
    "LRF",
    "2 3 E",
    "LRRF")) //io.Source.stdin.getLines())
}


