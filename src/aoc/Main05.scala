package aoc

import zio.*
import zio.stream.*
import java.nio.file.*

private type BigRange = (BigInt, BigInt)
private object BigRange:
  def contains(id: BigInt)(range: BigRange): Boolean = id >= range._1 && id <= range._2
  def merge(ranges: List[BigRange]): List[BigRange]  =
    if ranges.isEmpty then List.empty
    else
      val head :: tail = ranges.sortBy(_._1): @unchecked
      tail.foldLeft(head :: Nil) { case (merged @ mHead :: mTail, current @ (currentStart, currentEnd)) =>
        val lastStart -> lastEnd = mHead
        if currentStart <= lastEnd + 1 then lastStart -> lastEnd.max(currentEnd) :: mTail
        else current :: merged
      }

private class DB private (
  private val ranges: List[BigRange] = List.empty,
  private val items: Set[BigInt] = Set.empty
):
  def countFresh: Long      = items.count(id => ranges.exists(BigRange.contains(id)))
  def countElements: BigInt = BigRange.merge(ranges).map((start, end) => end - start + 1).sum

object DB:
  private def parseRange(raw: String): BigRange =
    val Array(start, end) = raw.split("-", 2).map(BigInt.apply); start -> end

  def parseFrom(path: Path): Task[DB] =
    val lines = ZStream.fromJavaIterator(Files.readAllLines(path).iterator()).map(_.trim)
    (lines.takeUntil(_.isEmpty).dropRight(1).map(parseRange).runCollect.map(_.toList) <&>
      lines.dropUntil(_.isEmpty).map(BigInt.apply).runCollect.map(_.toSet)).map(new DB(_, _))

object Main05 extends AOCApp:
  def program(path: Path) = for
    db <- DB.parseFrom(path)
    _   = println(s"Part 1: ${db.countFresh}")
    _   = println(s"Part 2: ${db.countElements}")
  yield ()
