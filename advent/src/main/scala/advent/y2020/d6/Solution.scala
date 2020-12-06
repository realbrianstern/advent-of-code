package advent.y2020.d6

import advent.Solver

import scala.util.Try

object Solution extends Solver {
  def year = 2020
  def day = 6

  def a =
    inputIterator
      .foldLeft(List(Set[Char]())) {
        case (groups, "") =>
          Set[Char]() :: groups
        case (head :: tail, line) =>
          line.toSet ++ head :: tail
        case _ =>
          throw new RuntimeException("Invalid input")
      }
      .map(_.size)
      .sum

  def b =
    inputIterator
      .foldLeft(List(Seq[Set[Char]]())) {
        case (groups, "") =>
          Seq[Set[Char]]() :: groups
        case (head :: tail, line) =>
          line.toSet +: head :: tail
        case _ =>
          throw new RuntimeException("Invalid input")
      }
      .map(_.reduce(_ & _))
      .map(_.size)
      .sum
}
