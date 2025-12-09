package aoc.day7

import aoc.AOCApp
import aoc.Ops.*
import zio.*
import zio.stream.ZStream

import java.nio.file.Path
import scala.collection.mutable
import scala.collection.concurrent
import scala.util.control.TailCalls.*

private type Position = (x: Long, y: Long)
private type Grid     = Map[Position, Char]

private def readGrid(path: Path): Task[Grid] =
  path.linesStream.zipWithIndex
    .map((line, r) => line.split("").zipWithIndex.map((char, c) => (r, c.toLong) -> char.head).toList)
    .flatMap(ZStream.fromIterable)
    .runCollect
    .map(Map.from)

private def findStart(grid: Grid): Option[Position] = grid.find(_._2 == 'S').map(_._1)

private def solve(grid: Grid, start: Position): Long =
  val beams = mutable.Queue(start)
  val seen  = mutable.Set(start)
  var count = 0L

  def add(pos: Position): Unit = if !seen.contains(pos) then
    seen.add(pos)
    beams.enqueue(pos)

  while beams.nonEmpty do
    val pos = beams.dequeue()
    grid
      .get(pos)
      .foreach:
        case '.' | 'S' if pos.x != grid.size - 1 => add(pos.x + 1, pos.y)
        case '^'                                 => count += 1; add(pos.x, pos.y - 1); add(pos.x, pos.y + 1)

  count

private def solve2(grid: Grid, start: Position): Long =
  val cache = mutable.Map.empty[Position, Long]

  def go(pos: Position): Long = cache.getOrElseUpdate(
    pos,
    grid.get(pos) match
      case Some('.' | 'S') => go(pos.x + 1, pos.y)
      case Some('^')       => go(pos.x, pos.y - 1) + go(pos.x, pos.y + 1)
      case None            => 1L
      case _               => 0L
  )

  go(start)

private def solve2x(grid: Grid, start: Position): Long =
  val cache = concurrent.TrieMap.empty[Position, Long]

  def go(pos: Position): TailRec[Long] = cache.get(pos) match
    case Some(v) => done(v)
    case None    =>
      grid.get(pos) match
        case Some('.' | 'S') => tailcall(go(pos.x + 1, pos.y)).map(cache.getOrElseUpdate(pos, _))
        case Some('^')       =>
          for l <- tailcall(go(pos.x, pos.y - 1)); r <- tailcall(go(pos.x, pos.y + 1))
          yield cache.getOrElseUpdate(pos, l + r)
        case None            => done(cache.getOrElseUpdate(pos, 1L))
        case _               => done(cache.getOrElseUpdate(pos, 0L))

  go(start).result

object Main07 extends AOCApp:

  def program(input: Path) = for
    grid  <- readGrid(input)
    start <- ZIO.getOrFail(findStart(grid))
    _      = println(s"Part 1: ${solve(grid, start)}")
    _      = println(s"Part 2: ${solve2x(grid, start)}")
  yield ()
