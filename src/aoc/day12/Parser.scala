package aoc.day12

import zio.{Chunk, Task}
import aoc.Ops.*

import java.nio.file.Path

object Parser:
  private def parseShape(line: Chunk[String]): (ID, Shape) = (
    line.head.dropRight(1).toInt,
    line.tail.zipWithIndex.flatMap((row, c) => row.zipWithIndex.collect { case '#' -> r => (c -> r) -> 1 }).toMap
  )

  private def parseRegion(line: String): Region =
    val List(dimensions, ids) = line.split(":", 2).toList.map(_.trim)
    val List(x, y)            = dimensions.split("x", 2).map(_.toInt).toList
    (x, y, ids.split(" ").map(_.toInt).toVector.zipWithIndex.map(_.swap).toMap)

  private val separator = """^\d+x\d+.*"""

  def parse(path: Path): Task[(Map[ID, Shape], List[Region])] =
    val lines                        = path.linesStream.map(_.trim)
    val shapes: Task[Map[ID, Shape]] = lines
      .takeWhile(!_.matches(separator))
      .mapAccum(Chunk.empty[String]) {
        case acc -> "" if acc.isEmpty => Chunk.empty   -> None
        case acc -> ""                => Chunk.empty   -> Some(acc)
        case (acc, line)              => (acc :+ line) -> None
      }
      .collectSome
      .map(parseShape)
      .runCollect
      .map(_.toMap)

    val regions = lines.filter(_.matches(separator)).map(parseRegion).runCollect
    shapes <&> regions.map(_.toList)
