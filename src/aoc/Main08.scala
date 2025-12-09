package aoc

import Ops.*
import zio.*
import zio.stream.*

import java.nio.file.*
import Math.{pow, sqrt}
import scala.collection.mutable

private type Position = (x: Int, y: Int, z: Int)
extension (a: Position)
  def distance(b: Position): Double =
    sqrt(pow(a.x - b.x, 2) + pow(a.y - b.y, 2) + pow(a.z - b.z, 2))

private val maxConnect: Int = 1000
private val largestCircuit  = 3

@scala.annotation.tailrec
private def findFrom(parents: mutable.Map[Position, Position])(p: Position): Position =
  val parent = parents(p)
  if parent == p then p
  else findFrom(parents)(parent)

private def solve(positions: List[Position]) =
  val distances = positions.combinations(2).collect { case List(a, b) => (a, b, a.distance(b)) }.toSeq.sortBy(_._3)
  val parents   = mutable.Map(positions.map(p => p -> p)*)
  val sizes     = mutable.Map(positions.map(p => p -> 1L)*)

  for (a, b, _) <- distances.take(maxConnect) do
    val (ra, rb) = findFrom(parents)(a) -> findFrom(parents)(b)
    if ra != rb then
      val (size1, size2) = sizes(ra) -> sizes(rb)
      if size1 < size2 then
        parents(ra) = rb; sizes(rb) = size1 + size2
      else
        parents(rb) = ra; sizes(ra) = size1 + size2

  val sizesX  = positions.map(findFrom(parents)).distinct.map(sizes).sortBy(-_)
  val result1 = sizesX.take(largestCircuit).product
  println(s"Part 1: $result1")

  // Part 2.
  var last: (Position, Position) = ((0, 0, 0), (0, 0, 0))
  for (a, b, _) <- distances.takeRight(distances.length - maxConnect) do
    val (ra, rb) = findFrom(parents)(a) -> findFrom(parents)(b)
    if ra != rb then
      val (size1, size2) = sizes(ra) -> sizes(rb)
      if size1 < size2 then
        parents(ra) = rb; sizes(rb) = size1 + size2
      else
        parents(rb) = ra; sizes(ra) = size1 + size2
      last = (a, b)

  val result2 = last._1.x.toLong * last._2.x.toLong
  println(s"Part 2: $result2")

object Main08 extends AOCApp:
  private def readPositions(path: Path): Task[List[Position]] =
    path.linesStream
      .map(_.trim)
      .map { r =>
        val List(x, y, z) = r.split(",", 3).map(_.toInt).toList; (x, y, z)
      }
      .runCollect
      .map(_.toList)

  def program(path: Path) = for
    positions <- readPositions(path)
    _          = solve(positions)
  yield ()
