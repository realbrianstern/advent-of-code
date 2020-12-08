package advent.y2020.d7

import advent.Solver

object Solution extends Solver {
  def year = 2020
  def day = 7

  type RuleId = String
  type RuleContents = Map[RuleId, Int]
  type RuleSet = Map[RuleId, RuleContents]

  def parseRule(line: String): (RuleId, RuleContents) = {
    val headParser =
      """^(\w+ \w+) bags contain (no|\d+) (\w+ ?\w*) bags?.?$""".r
    val tailParser = """^(\d+) (\w+ \w+) bags?.?$""".r
    val sections = line.split(", ")

    sections.head match {
      case headParser(ruleId, containedQuantity, containedRuleId) =>
        containedQuantity match {
          case "no" =>
            (ruleId, Map.empty)
          case _ =>
            val contents = Map(containedRuleId -> containedQuantity.toInt) ++
              sections
                .tail
                .map {
                  case tailParser(sectionQuantity, sectionRuleId) =>
                    (sectionRuleId, sectionQuantity.toInt)
                }

            (ruleId, contents)
        }
    }
  }

  val input: RuleSet = inputIterator.map(parseRule).toMap

  def getContainingRules(ruleId: RuleId, ruleSet: RuleSet): Set[RuleId] = {
    val ruleIds =
      ruleSet
        .collect {
          case (id, contents) if contents.contains(ruleId) =>
            id
        }
        .toSet

    ruleIds ++ ruleIds.flatMap(getContainingRules(_, ruleSet))
  }

  def a = getContainingRules("shiny gold", input).size

  def getContainedCounts(ruleId: RuleId, ruleSet: RuleSet): Int =
    ruleSet
      .get(ruleId)
      .get
      .foldLeft(1) {
        case (total, (ruleId, quantity)) =>
          total + quantity * getContainedCounts(ruleId, ruleSet)
      }

  def b = getContainedCounts("shiny gold", input) - 1
}
