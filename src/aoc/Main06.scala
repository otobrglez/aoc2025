package aoc
import aoc.Ops.*
import zio.*

import java.nio.file.*
import scala.collection.mutable

private type Operand = Char
private val operands: Set[Operand] = Set('*', '+')

private def part1(path: Path) =
  val tokens = path.linesStream.map(_.trim).map(_.split(" ").map(_.trim).toList.filter(_.nonEmpty))

  for
    numbers <-
      tokens
        .takeUntil(_.exists(t => operands.contains(t.head)))
        .dropRight(1)
        .map(_.map(r => BigInt(r)).zipWithIndex.map(_.swap).toMap)
        .zipWithIndex
        .map(_.swap)
        .runCollect
        .map(_.toMap)

    maybeOperations <-
      tokens.filter(_.exists(t => operands.contains(t.head))).map(_.zipWithIndex.map(_.swap).toMap).runHead
    operations      <- ZIO.getOrFail(maybeOperations)
  yield operations.keys.foldLeft(BigInt(0)) { case (acc, i) =>
    val xs = numbers.map(_._2(i))
    acc + {
      operations.get(i).flatMap(_.headOption) match
        case Some('*') => xs.product
        case Some('+') => xs.sum
        case s         => throw new Exception(s"Invalid operation: $s")
    }
  }

// part 2
private def sliceColumns(lines: List[String]): List[List[String]] =
  if lines.isEmpty then return Nil

  val width      = lines.map(_.length).max
  val padded     = lines.map(_.padTo(width, ' '))
  val isBoundary = (0 until width).map(col => padded.forall(_(col) == ' ')).toVector
  val ranges     =
    val buf   = mutable.ListBuffer.empty[(Int, Int)]
    var start = -1

    for i <- 0 until width do
      if !isBoundary(i) && start == -1 then start = i
      if (isBoundary(i) || i == width - 1) && start != -1 then
        val end = if isBoundary(i) then i else i + 1
        buf += start -> end
        start = -1
    buf.toList

  padded.map(line => ranges.map(line.substring(_, _)))

private def sliceRightToLeft(lines: List[String]): List[String] =
  if lines.isEmpty then return Nil

  val width  = lines.map(_.length).max
  val padded = lines.map(_.reverse.padTo(width, ' ').reverse)

  (0 until width).reverse.toList.flatMap: col =>
    val digits = padded.flatMap(_(col).toString.filter(_.isDigit))
    if digits.nonEmpty then Some(digits.mkString)
    else None

private def processColumn(lines: List[String]) =
  val List(tokens, opList) = lines.splitAt(lines.length - 1).toList
  val numbers              = sliceRightToLeft(tokens).map(BigInt(_))

  opList.headOption.flatMap(_.headOption) match
    case Some('*') => numbers.product
    case Some('+') => numbers.sum
    case op        => throw new Exception(s"Invalid operand: $op")

private def part2(path: Path) = for
  rows   <- path.linesStream.runCollect.map(_.toList).map(sliceColumns)
  columns = rows.transpose.reverse.map(processColumn)
yield columns.sum

object Main06 extends AOCApp:

  def program(path: Path) = for
    _ <- part1(path).debug("Part 1")
    _ <- part2(path).debug("Part 2")
  yield ()
