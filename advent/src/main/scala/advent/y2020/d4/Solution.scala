package advent.y2020.d4

import advent.Solver

import scala.util.Try

object Solution extends Solver {
  def year = 2020
  def day = 4

  type Passport = Map[String, String]

  val input: List[Passport] =
    inputIterator.foldLeft(List(Map[String, String]())) {
      case (passports, "") =>
        Map[String, String]() :: passports
      case (head :: tail, line) =>
        line
          .split(" ")
          .map(_.split(":"))
          .foldLeft(head) {
            case (passport, Array(key, value)) =>
              passport + (key -> value)
          } :: tail
      case _ =>
        throw new RuntimeException("Invalid input")
    }

  def a = {
    val requiredFields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

    input.count(passport => requiredFields.subsetOf(passport.keySet))
  }

  def b =
    input.count(passport =>
      Seq(
        passport.get("byr").map(validateByr),
        passport.get("iyr").map(validateIyr),
        passport.get("eyr").map(validateEyr),
        passport.get("hgt").map(validateHgt),
        passport.get("hcl").map(validateHcl),
        passport.get("ecl").map(validateEcl),
        passport.get("pid").map(validatePid)
      ).forall(_.contains(true))
    )

  def validateByr(value: String) =
    isDigits(value) && value.length() == 4 && isInRange(value, 1920, 2002)

  def validateIyr(value: String) =
    isDigits(value) && value.length() == 4 && isInRange(value, 2010, 2020)

  def validateEyr(value: String) =
    isDigits(value) && value.length() == 4 && isInRange(value, 2020, 2030)

  def validateHgt(value: String) =
    value.splitAt(value.length() - 2) match {
      case (hgtStr, "cm") =>
        isDigits(hgtStr) && isInRange(hgtStr, 150, 193)
      case (hgtStr, "in") =>
        isDigits(hgtStr) && isInRange(hgtStr, 59, 76)
      case _ =>
        false
    }

  def validateHcl(value: String) =
    value.head == '#' && value.length() == 7 &&
      "^[a-f0-9]*$".r.matches(value.tail)

  def validateEcl(value: String) =
    Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(value)

  def validatePid(value: String) = isDigits(value) && value.length() == 9

  def isDigits(value: String): Boolean = Try(value.toInt).isSuccess

  def isInRange(value: String, min: Int, max: Int) =
    Range.inclusive(min, max).contains(value.toInt)
}
