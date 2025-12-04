package aoc

import zio.*
import zio.Console.printLine
import zio.stream.*

import java.nio.file.*

type Roll = Char
val roll: Roll = '@'; val space: Roll = '.'
type Row = Long; type Column = Long

final class Grid private (private val grid: Map[(Row, Column), Option[Roll]] = Map.empty):
  def find(r: Row, c: Column): Option[Roll]                     = grid.get((r, c)).flatten
  def size: (Row, Column)                                       = grid.keys.map(_._1).max -> grid.keys.map(_._2).max
  private def iterator: Iterator[((Row, Column), Option[Roll])] = grid.iterator

  private def offsets(r: Row, c: Column): List[(Row, Column)] =
    List(
      // format: off
      (r - 1, c - 1), (r - 1, c), (r - 1, c + 1),
      (r, c - 1), (r, c + 1),
      (r + 1, c - 1), (r + 1, c), (r + 1, c + 1)
      // format: on
    )

  private def countNeighbors(r: Row, c: Column): Int =
    offsets(r, c).count((r, c) => find(r, c).contains(roll))

  def numberOfAccessible(): Long =
    iterator.collect { case (pos @ (r, c), Some(`roll`)) => pos -> countNeighbors(r, c) }.count((_, count) => count < 4)

  def mapAccessible(f: (Row, Column) => Option[Roll]): Seq[((Row, Column), Option[Roll])] = iterator.toSeq.map:
    case (pos @ (r, c), v @ Some(`roll`)) if countNeighbors(r, c) < 4 => pos -> f(r, c)
    case pos -> v                                                     => pos -> v

  def withoutAccessible: Grid = Grid.from(mapAccessible((_, _) => None))

object Grid:
  def fromPath(path: Path): Task[Grid] = {
    for
      line -> r <- ZStream.fromJavaIterator(Files.readAllLines(path).iterator()).map(_.trim).zipWithIndex
      char -> c <- ZStream.fromIterable(line.zipWithIndex)
    yield r -> c.toLong -> Option.when(char != space)(roll)
  }.runCollect.map(Map.from).map(Grid.apply)

  def from(seq: Seq[((Row, Column), Option[Roll])]) = new Grid(Map.from(seq))

  def print(grid: Grid): Task[Unit] =
    val (rows, cols) = grid.size
    ZIO.foreachDiscard(0L to rows)(r => printLine((0L to cols).map(c => grid.find(r, c).getOrElse(space)).mkString))

  def removableCount(grid: Grid): Long =
    @annotation.tailrec
    def loop(g: Grid, acc: Long): Long =
      val n = g.numberOfAccessible()
      if n > 0 then loop(g.withoutAccessible, acc + n) else acc
    loop(grid, 0L)

object Main04 extends AOCApp:
  def program(path: Path) = for
    grid              <- Grid.fromPath(path).tap(Grid.print)
    // Part 1: Number of neighbour roles less than 4
    numberOfPaperRoles = grid.numberOfAccessible()
    _                  = println(numberOfPaperRoles)
    // Part 2: Number of removable roles
    removableCount     = Grid.removableCount(grid)
    _                  = println(removableCount)
  yield ()
