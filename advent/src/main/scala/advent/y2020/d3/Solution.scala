package advent.y2020.d3

import advent.Solver

import scala.util.chaining._

object Solution extends Solver {
  def year = 2020
  def day = 3

  def countTrees(stepsRight: Int, stepsDown: Int) =
    inputIterator
      .zipWithIndex
      .filter(_._2 % stepsDown == 0)
      .map(_._1)
      .foldLeft((0, 0)) {
        case ((x, treesCount), row) if (row.charAt(x % row.length()) == '#') =>
          (x + stepsRight, treesCount + 1)
        case ((x, treesCount), _) =>
          (x + stepsRight, treesCount)
      }
      .pipe(_._2)

  def a = countTrees(3, 1)

  def b =
    Seq((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
      .map(coords => countTrees(coords._1, coords._2))
      .map(_.toLong)
      .product
}
