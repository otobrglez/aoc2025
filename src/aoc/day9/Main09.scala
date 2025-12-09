package aoc.day9

import aoc.AOCApp
import aoc.Ops.*
import zio.*

import java.nio.file.Path

private type Position = (x: Long, y: Long)
extension (a: Position) def area(b: Position): Long = (Math.abs(b.x - a.x) + 1) * (Math.abs(b.y - a.y) + 1)

def readPositions(path: Path): Task[List[Position]] =
  path.linesStream
    .map(_.split(",", 2).toList.map(_.toLongOption))
    .collect[Position] { case List(Some(x), Some(y)) => x -> y }
    .runCollect
    .map(_.toList)

def findLargestArea(positions: List[Position]): Option[(Position, Position, Long)] =
  val pairs = for i <- positions.indices; j <- (i + 1) until positions.size yield positions(i) -> positions(j)
  pairs.map((p1, p2) => (p1, p2, p1.area(p2))).maxByOption(_._3)

def isInside(pt1: Position, pt2: Position, polygon: List[Position]): Boolean =
  val (xmin, xmax) = Math.min(pt1.x, pt2.x) -> Math.max(pt1.x, pt2.x)
  val (ymin, ymax) = Math.min(pt1.y, pt2.y) -> Math.max(pt1.y, pt2.y)

  val edges = polygon.indices.map(i => (polygon(i), polygon((i + 1) % polygon.length)))
  edges.forall: (p1, p2) =>
    val ((x1, y1), (x2, y2)) = p1 -> p2
    if y1 == y2 then
      !(ymin < y1 && y1 < ymax &&
        (Math.min(x1, x2) <= xmin && xmin < Math.max(x1, x2) || Math.min(x1, x2) < xmax && xmax <= Math.max(x1, x2)))
    else if x1 == x2 then
      !(xmin < x1 && x1 < xmax &&
        (Math.min(y1, y2) <= ymin && ymin < Math.max(y1, y2) || Math.min(y1, y2) < ymax && ymax <= Math.max(y1, y2)))
    else throw new AssertionError("Expected to be rectilinear.")

def findLargestArea2(polygon: List[Position]): Option[Long] =
  polygon
    .combinations(2)
    .collect { case List(p1, p2) if isInside(p1, p2, polygon) => p1.area(p2) }
    .maxOption

object Main09 extends AOCApp:
  def program(path: Path) = for
    positions <- readPositions(path)
    _          = findLargestArea(positions).foreach((_, _, area) => println(s"Part 1: $area"))
    _          = findLargestArea2(positions).foreach(area => println(s"Part 2: $area"))
  yield ()
