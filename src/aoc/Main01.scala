package aoc

import zio.*
import zio.Console.*
import zio.stream.*
import java.nio.file.*

object Main01 extends AOCApp:
  private val (leftMatch, rightMatch) = ("""^L(\d+)$""".r, """^R(\d+)$""".r)

  private def zeros(p: Int, d: Int): Int =
    (Math.abs(d) + (if d < 0 && p != 0 then 100 - p else p)) / 100

  def program(path: Path) = for
    rs     <-
      ZStream
        .fromJavaIterator(Files.readAllLines(path).iterator())
        .map(_.trim)
        .map {
          case leftMatch(dir)  => dir.toInt
          case rightMatch(dir) => -dir.toInt
        }
        .runCollect

    // Part 1: Zeroes
    ps      = rs.scanLeft(50)((p, d) => Math.floorMod(p + d, 100))
    result1 = ps.count(_ == 0)
    _      <- printLine(s"Part 1: $result1")

    // Part 2: More zeroes...
    result2 = ps.zip(rs).map(zeros).sum
    _      <- printLine(s"Part 2: $result2")
  yield ()
