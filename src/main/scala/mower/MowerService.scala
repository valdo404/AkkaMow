package mower

import scala.collection.{IndexedSeq, Iterator}

/** Mower service is a service whose goal is to move mowers */
object MowerService {
  /**
   * service execution
   * @param args does not anything
   */
  def main(args: Array[String]) {
    println("Beginning mowing")

    MowerProgrammingService.execute(io.Source.stdin.getLines)

    println("Mowed terminated")
  }





}

/**
 * Mower programming service is a singleton whose goal is parsing flat files
 * it also build lawn and mowers. After that it takes mowers and make them move
 */
object MowerProgrammingService {
  /**
   * Execute the service on several lines
   *
   * @param lines the configuration lines
   */
  def execute(lines: Iterator[String]) {
    val lawn = parseLawn(lines.next())

    while(lines.hasNext) {
      val mower = parseMower(lawn, lines.next())
      val instructions = parseMowerInstructions(lines.next())

      for(instruction <- instructions)
        mower.operate(instruction)

      mower.tell()
    }
  }

  /**
   * Builds a lawn from a lawn configuration line
   * @param line the lawn line
   * @return a Lawn
   */
  def parseLawn(line: String): Lawn = {
    val values = line.split(' ')

    new Lawn((values(0).toInt, values(1).toInt))
  }

  /**
   * Builds a mower from a mower configuration line
   * @param line the mower description line
   * @return a Mower
   */
  def parseMower(lawn: Lawn, line: String): Mower = {
    val values = line.split(' ')
    val orientation = Orientation.withName(values(2).toString)

    new Mower(lawn, values(0).toInt, values(1).toInt, orientation)
  }

  /**
   * Builds mower instructions from a mower instructions line
   * @param line the mower instructions line
   * @return list of mower Operation
   */
  def parseMowerInstructions(line: String): IndexedSeq[Operation.Value] = {
    for(current<-line) yield Operation.withName(current.toString)
  }
}