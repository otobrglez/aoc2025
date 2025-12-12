package aoc.day11

import aoc.Ops.*
import zio.*

import java.nio.file.Path
import scala.collection.mutable

type Label = String
type Rack  = (label: Label, outputs: Vector[Label])
type Racks = Map[Label, Seq[Label]]

private def parse(path: Path): Task[Racks] = path.linesStream
  .map(_.split(" ").map(_.replace(":", "")).toVector)
  .collect { case Vector(label, outputs*) => label -> outputs }
  .runCollect
  .map(_.toMap)

private def findPathsFromPar(racks: Racks, start: Label, end: Label): Task[Vector[Vector[Label]]] =
  def dfs(current: Label, visited: Set[Label], path: List[Label]): Vector[Vector[Label]] =
    if current == end then Vector(path.reverse.toVector)
    else if visited.contains(current) then Vector.empty
    else
      racks.get(current) match
        case None            => Vector.empty
        case Some(neighbors) =>
          neighbors.flatMap(next => dfs(next, visited + current, next :: path)).toVector

  ZIO.succeed(dfs(start, Set.empty, List(start)))

private def findPathsFrom(racks: Racks, start: Label, end: Label): Vector[Vector[Label]] =
  val labels   = (racks.keys.toSet + end).toVector
  val labelIdx = labels.zipWithIndex.toMap
  val idxLabel = labels

  val graph: Array[Array[Int]] = Array.tabulate(labels.size): i =>
    racks.getOrElse(idxLabel(i), Seq.empty).flatMap(labelIdx.get).toArray

  val (startIdx, endIdx) = labelIdx.getOrElse(start, -1) -> labelIdx.getOrElse(end, -1)
  if startIdx < 0 || endIdx < 0 then return Vector.empty

  val visited = new java.util.BitSet(labels.size)
  val results = Vector.newBuilder[Vector[Label]]

  def dfs(current: Int, path: List[Int]): Unit =
    if current == endIdx then results += path.reverse.map(idxLabel).toVector
    else if !visited.get(current) then
      visited.set(current)
      for next <- graph(current) do dfs(next, next :: path)
      visited.clear(current) // backtrack

  dfs(startIdx, List(startIdx))
  results.result()

private def countPaths(
  racks: Racks,
  start: Label,
  end: Label,
  mustPass: Set[Label] = Set.empty
): Long =
  val allNodes      = racks.keys.toSet + end
  val mustPassList  = mustPass.toVector
  val mustPassIdx   = mustPassList.zipWithIndex.toMap
  val mustPassCount = mustPassList.size
  val fullMask      = (1 << mustPassCount) - 1

  def nodeMask(node: Label): Int = mustPassIdx.get(node).map(1 << _).getOrElse(0)

  val inDegree = mutable.Map[Label, Int]().withDefaultValue(0)
  for (_, outputs) <- racks; out <- outputs do inDegree(out) += 1

  val queue     = mutable.Queue[Label](allNodes.filter(inDegree(_) == 0).toSeq*)
  val topoOrder = mutable.ArrayBuffer[Label]()

  while queue.nonEmpty do
    val node = queue.dequeue()
    topoOrder += node
    for next <- racks.getOrElse(node, Seq.empty) do
      inDegree(next) -= 1
      if inDegree(next) == 0 then queue.enqueue(next)

  // Paths from `node` to `end`
  val dp = mutable.Map[(Label, Int), Long]().withDefaultValue(0L)

  for node <- topoOrder.reverseIterator do
    val myBit = nodeMask(node)

    for incomingMask <- 0 until (1 << mustPassCount) do
      val maskAtNode = incomingMask | myBit
      val count      =
        if node == end then if maskAtNode == fullMask then 1L else 0L
        else racks.getOrElse(node, Seq.empty).map(n => dp(n -> maskAtNode)).sum

      if count > 0 then dp(node -> incomingMask) = count

  dp(start -> 0)

object Main11 extends aoc.AOCApp:
  def program(path: Path): Task[Unit] = for
    out <- parse(path).debugWith(r => s"Racks: ${r.size}")

    (d1, numberOfPaths) <- ZIO.succeed(findPathsFrom(out, "you", "out").length).timed
    _                   <- zio.Console.printLine(s"Part 1: $numberOfPaths, Time: ${d1.toMillis}ms")

    (d2, numberOfPaths2) <- ZIO.succeed(countPaths(out, "svr", "out", mustPass = Set("dac", "fft"))).timed
    _                    <- zio.Console.printLine(s"Part2: $numberOfPaths2, Time: ${d2.toMillis}ms")
  yield ()
