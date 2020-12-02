package advent

import java.io.FileNotFoundException
import scala.io.Source
import scala.util.Try

trait Solver {
  def year: Int
  def day: Int

  def inputIterator: Iterator[String] = {
    val resourcePath = s"$year/$day.txt"

    Try(Source.fromResource(resourcePath).getLines())
      .recover(_ => throw new FileNotFoundException(resourcePath))
      .get
  }

  def a: Any

  def b: Any
}
