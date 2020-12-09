package advent.y2020.d8

import advent.Solver

import scala.util.chaining._

object Solution extends Solver {
  def year = 2020
  def day = 8

  val input: Vector[Instruction] =
    inputIterator.map(Instruction.parseLine).toVector

  def runProgram(program: Vector[Instruction]): Either[Int, Int] = {
    var accumulator = 0
    var index = 0
    var visitedIndices = Set[Int]()

    while (!visitedIndices.contains(index)) {
      if (index >= program.length) {
        return Right(accumulator)
      }

      visitedIndices += index

      program(index) match {
        case NOP(_) =>
          index += 1
        case ACC(value) =>
          accumulator += value
          index += 1
        case JMP(value) =>
          index += value
      }
    }

    Left(accumulator)
  }

  def a =
    runProgram(input)
      .swap
      .getOrElse(throw new RuntimeException("Invalid input"))

  def flipInput(index: Int): Vector[Instruction] =
    input.updated(
      index,
      input(index) match {
        case flippable: Flippable =>
          flippable.flip
        case _ =>
          throw new RuntimeException("Unflippable instruction")
      }
    )

  def b =
    input
      .zipWithIndex
      .collect {
        case (_: Flippable, index) =>
          index
      }
      .dropWhile(flipInput(_).pipe(runProgram).pipe(_.isLeft))
      .head
      .pipe(flipInput)
      .pipe(runProgram)
      .getOrElse(throw new RuntimeException("Invalid input"))
}

sealed trait Instruction
sealed trait Flippable {
  def flip: Instruction
}

case class ACC(value: Int) extends Instruction
case class NOP(value: Int) extends Instruction with Flippable {
  def flip = JMP(value)
}
case class JMP(value: Int) extends Instruction with Flippable {
  def flip = NOP(value)
}

object Instruction {
  val parser = """^(nop|acc|jmp) ([-+]\d+)$""".r

  def parseLine(line: String): Instruction =
    line match {
      case parser(operation, argument) =>
        val argumentInt = argument.toInt
        operation match {
          case "nop" =>
            NOP(argumentInt)
          case "acc" =>
            ACC(argumentInt)
          case "jmp" =>
            JMP(argumentInt)
        }
    }
}
