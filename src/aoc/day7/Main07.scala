package aoc.day7

import aoc.AOCApp
import zio.*
import aoc.Ops.*

import java.nio.file.Path

private type Symbol = Char
private val empty: Symbol        = '.'
private val splitter: Symbol     = '^'
private val symbols: Set[Symbol] = Set(empty, 'S', splitter)
private type Row      = Long; private type Column = Long
private type Position = (Row, Column)

final private case class Grid private (
  rows: Map[Position, Symbol] = Map.empty
):
  def start: Option[Position] = rows.collectFirst { case (pos, 'S') => pos }

private object Grid:
  def from(path: Path): Task[Grid] =
    path.linesStream.zipWithIndex
      .map((r, i) => r.zipWithIndex.map((v, c) => (i, c.toLong) -> v))
      .runCollect
      .map(_.toList.flatten.toMap)
      .map(Grid(_))

object Main07 extends AOCApp:

  def program(input: Path) = for
    _ <- zio.Console.printLine("go go go...")
    g <- Grid.from(input)
    _  = println(g)
    _  = g.start.foreach(println)
    _ <- zio.Console.printLine("go go go...")
  yield ()
