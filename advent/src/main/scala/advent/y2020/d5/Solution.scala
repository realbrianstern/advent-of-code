package advent.y2020.d5

import advent.Solver

import scala.util.chaining._

object Solution extends Solver {
  def year = 2020
  def day = 5

  def findSeat(line: String): Seat = {
    val (rowChars, colChars) = line.splitAt(7)

    val row =
      rowChars
        .foldLeft((0, 127)) {
          case ((min, max), 'F') =>
            (min, (max + min) / 2)
          case ((min, max), 'B') =>
            ((min + max) / 2 + 1, max)
          case _ =>
            throw new RuntimeException("Invalid input")
        }
        ._1

    val col =
      colChars
        .foldLeft((0, 7)) {
          case ((min, max), 'L') =>
            (min, (max + min) / 2)
          case ((min, max), 'R') =>
            ((min + max) / 2 + 1, max)
          case _ =>
            throw new RuntimeException("Invalid input")
        }
        ._1

    Seat(row, col)
  }

  val input = inputIterator.map(findSeat)

  def a = input.map(_.id).max

  def b = {
    val allSeats =
      Range
        .inclusive(1, 126)
        .flatMap(row => Range.inclusive(0, 7).map(Seat(row, _)))
        .toSet

    val emptySeats = allSeats -- input

    emptySeats.groupBy(_.row).map(_._2).filter(_.size == 1).head.head.id
  }
}

case class Seat(row: Int, col: Int) {
  def id = (row * 8) + col
}
