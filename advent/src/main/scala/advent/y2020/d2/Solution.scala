package advent.y2020.d2

import advent.Solver

object Solution extends Solver {
  def year = 2020
  def day = 2

  val input: Iterator[PasswordLine] = inputIterator
    .map(inputRow => PasswordLine.fromString(inputRow))

  def a =
    input.count(pl =>
      Range
        .inclusive(pl.lowNumber, pl.highNumber)
        .contains(pl.password.count(_ == pl.keyChar))
    )

  def b =
    input.count { pl =>
      val atLow = pl.password.charAt(pl.lowNumber - 1) == pl.keyChar
      val atHigh = pl.password.charAt(pl.highNumber - 1) == pl.keyChar

      (atLow || atHigh) && !(atLow && atHigh)
    }
}

case class PasswordLine(
    keyChar: Char,
    lowNumber: Int,
    highNumber: Int,
    password: String
)

object PasswordLine {
  def fromString(input: String): PasswordLine = {
    // "lowNumber-highNumber keyChar: password"
    val Array(rawPolicy, rawPassword) = input.split(":")
    val Array(rawNumbers, rawKeyChar) = rawPolicy.split(" ")
    val Array(rawLowNumber, rawHighNumber) = rawNumbers.split("-")

    PasswordLine(
      rawKeyChar.head,
      rawLowNumber.toInt,
      rawHighNumber.toInt,
      rawPassword.tail
    )
  }
}
