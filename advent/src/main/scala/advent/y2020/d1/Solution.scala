package advent.y2020.d1

import advent.Solver

object Solution extends Solver {
  def year = 2020
  def day = 1

  val input: Seq[Int] = inputIterator.toSeq.map(_.toInt)

  def a = input.combinations(2).find(_.sum == 2020).map(_.product)

  def b = input.combinations(3).find(_.sum == 2020).map(_.product)
}
