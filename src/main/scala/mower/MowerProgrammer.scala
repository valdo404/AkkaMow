package mower

/**
 * Mower programming service is a singleton whose goal is parsing flat files
 * it also build lawn and mowers. After that it takes mowers and make them move
 */
object MowerProgrammer {
  def execute(lines: Iterator[String]) {
    val lawn = MowerParser.lawn(lines.next()) fold (lawn => lawn, error => throw new Exception(error))

    while(lines.hasNext) {
      val mower = MowerParser.mower(lawn, lines.next()) fold (
        possible => possible, error => throw new Exception(error))
      val instructions = MowerParser.instructions(lines.next())

      for(instruction <- instructions)
        instruction fold (instruction => mower(instruction), error => throw new Exception(error))

      mower.tell()
    }
  }
}
