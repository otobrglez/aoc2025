package aoc.day12

import zio.*
import zio.Console.printLine

import java.nio.file.Path
import scala.collection.mutable
import scala.util.hashing.MurmurHash3

private type ID        = Int
private type Position  = (x: Int, y: Int)
private type Shape     = Map[Position, Int]
private type Region    = (width: Int, height: Int, counts: Map[ID, Int])
private type Placement = (id: ID, mask: Array[Long], cells: Array[Int])

object Shape:
  def size(shape: Shape): (Int, Int) =
    shape.keys.maxBy(_.x).x + 1 -> (shape.keys.maxBy(_.y).y + 1)

  def draw(shape: Shape): Unit =
    val (w, h) = size(shape)
    for r <- 0 until h do
      for c <- 0 until w do print(if shape.contains(r -> c) then '#' else '.')
      println()

  private def normalize(shape: Shape): Shape =
    val (xs, ys)     = shape.keys.map(_.x) -> shape.keys.map(_.y)
    val (minX, minY) = xs.min              -> ys.min
    shape.map { case ((x, y), v) => ((x - minX, y - minY), v) }

  private def rotate90(shape: Shape): Shape = normalize(shape.map { case ((x, y), v) => ((y, -x), v) })
  private def flipX(shape: Shape): Shape    = normalize(shape.map { case ((x, y), v) => ((-x, y), v) })

  private def rotations(shape: Shape): List[Shape] =
    val s0 = normalize(shape)
    val s1 = rotate90(s0)
    val s2 = rotate90(s1)
    val s3 = rotate90(s2)
    List(s0, s1, s2, s3)

  def variations(shape: Shape): Vector[Shape] =
    val rs  = rotations(shape)
    val frs = rotations(flipX(shape))
    (rs ++ frs).toSet.toVector

private def solve(shapes: Map[ID, Shape], regions: List[Region]): Int =
  regions.map(canPlace(shapes, _)).collect { case Some(_) => 1 }.size

private def canPlace(
  shapes: Map[ID, Shape],
  region: Region
): Option[Map[Position, ID]] =
  val (width, height, counts0)        = region
  val cellCount                       = width * height
  val wordCount                       = (cellCount + 63) >>> 6
  inline def idx(x: Int, y: Int): Int = y * width + x
  inline def word(i: Int): Int        = i >>> 6
  inline def bit(i: Int): Long        = 1L << (i & 63)

  val ids: Vector[ID] = counts0.keys.toVector.sorted

  def buildPlacement(id: ID, cells: Array[Position], dx: Int, dy: Int): Placement =
    val maskArr = Array.fill[Long](wordCount)(0L)
    val idxs    = new Array[Int](cells.length)
    var i       = 0
    while i < cells.length do
      val (x0, y0) = cells(i)
      val ci       = idx(x0 + dx, y0 + dy)
      maskArr(word(ci)) |= bit(ci)
      idxs(i) = ci
      i += 1

    (id, maskArr, idxs)

  val placementsById = ids.map { id =>
    val vars = Shape.variations(shapes(id))
    val ps   = Vector.newBuilder[Placement]

    vars.foreach { v =>
      val (sw, sh) = Shape.size(v)
      if sw <= width && sh <= height then
        val cells = v.keysIterator.toArray
        var dy    = 0
        while dy <= (height - sh) do
          var dx = 0
          while dx <= (width - sw) do
            ps += buildPlacement(id, cells, dx, dy)
            dx += 1
          dy += 1
    }
    ps.result()
  }.toArray

  val idOrder: Vector[ID]     = ids.sortBy(id => placementsById(ids.indexOf(id)).size)
  val idToIndex: Map[ID, Int] = ids.zipWithIndex.toMap
  val remaining: Array[Int]   = ids.map(counts0.getOrElse(_, 0)).toArray

  val requiredArea: Int = remaining.zip(ids).map { case (n, id) => n * shapes(id).size }.sum
  if requiredArea > cellCount then return None

  val occupancy: Array[Long] = Array.fill(wordCount)(0L)

  val (undoWords, undoMarks) = new mutable.ArrayBuffer[(Int, Long)](1024) -> new mutable.ArrayBuffer[Int](256)

  def intersects(mask: Array[Long]): Boolean =
    var i = 0
    while i < wordCount do
      if (occupancy(i) & mask(i)) != 0L then return true
      i += 1
    false

  inline def applyMask(mask: Array[Long]): Unit =
    undoMarks += undoWords.size
    var i = 0
    while i < wordCount do
      val m = mask(i)
      if m != 0L then
        val old = occupancy(i)
        val nw  = old | m
        if nw != old then
          undoWords += ((i, old))
          occupancy(i) = nw
      i += 1

  def rollback(): Unit =
    val mark = undoMarks.remove(undoMarks.size - 1)
    var k    = undoWords.size - 1
    while k >= mark do
      val (i, old) = undoWords(k)
      occupancy(i) = old
      k -= 1

    undoWords.dropRightInPlace(undoWords.size - mark)

  // memoization
  val dead = new java.util.HashSet[Long](1 << 20)

  def stateHash(): Int =
    var h    = 0x1234abcd // seed
    var nMix = 0

    var i = 0
    while i < wordCount do
      val w  = occupancy(i)
      val lo = w.toInt
      val hi = (w >>> 32).toInt
      h = MurmurHash3.mix(h, lo);
      nMix += 1
      h = MurmurHash3.mix(h, hi);
      nMix += 1
      i += 1

    i = 0
    while i < remaining.length do
      h = MurmurHash3.mix(h, remaining(i));
      nMix += 1
      i += 1

    MurmurHash3.finalizeHash(h, nMix)

  def allPlaced: Boolean =
    var i = 0
    while i < remaining.length do
      if remaining(i) > 0 then return false
      i += 1
    true

  def pickNextId(): Int =
    var (bestIdx, bestCount) = -1 -> Int.MaxValue

    var k = 0
    while k < idOrder.length do
      val id   = idOrder(k)
      val iIdx = idToIndex(id)
      if remaining(iIdx) > 0 then
        val ps      = placementsById(iIdx)
        var (ok, j) = 0 -> 0
        while j < ps.length && ok < bestCount do
          if !intersects(ps(j).mask) then ok += 1
          j += 1
        if ok == 0 then return -1 // immediate dead end
        if ok < bestCount then
          bestCount = ok
          bestIdx = iIdx
      k += 1

    bestIdx

  val chosen = new mutable.ArrayBuffer[Placement](256)

  def search(): Boolean =
    if allPlaced then return true

    val h = stateHash()
    if dead.contains(h) then return false

    val nextIdx = pickNextId()
    if nextIdx < 0 then
      dead.add(h); return false

    val ps = placementsById(nextIdx)
    var j  = 0
    while j < ps.length do
      val p = ps(j)
      if !intersects(p.mask) then
        applyMask(p.mask)
        remaining(nextIdx) -= 1
        chosen += p

        if search() then return true

        chosen.remove(chosen.size - 1)
        remaining(nextIdx) += 1
        rollback()
      j += 1

    dead.add(h)
    false

  if !search() then None
  else
    val out = mutable.Map.empty[Position, ID]
    chosen.foreach { p =>
      val id = p.id
      var i  = 0
      while i < p.cells.length do
        val ci       = p.cells(i)
        val position = ci % width -> (ci / width)
        out.update(position, id)
        i += 1
    }
    Some(out.toMap)

object Main12 extends aoc.AOCApp:
  def program(path: Path) =
    Parser.parse(path).map(solve).tap(s => printLine(s"Part 1: $s")).unit
