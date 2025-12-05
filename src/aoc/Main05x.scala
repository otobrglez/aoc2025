package aoc

import zio.*
import zio.Console.*
import zio.stream.*

import java.nio.file.*
import scala.collection.immutable.NumericRange

object Main05x extends AOCApp:
  private def parseBigInt(raw: String): (BigInt, BigInt) =
    val Array(start, end) = raw.split("-", 2).map(BigInt.apply); start -> end

  def parseFrom(path: Path): Task[(List[(BigInt, BigInt)], Set[BigInt])] =
    val lines = ZStream.fromJavaIterator(Files.readAllLines(path).iterator()).map(_.trim)
    lines.takeUntil(_.isEmpty).dropRight(1).map(parseBigInt).runCollect.map(_.toList) <&>
      lines.dropUntil(_.isEmpty).map(BigInt.apply).runCollect.map(_.toSet)

  def program(path: Path) = for
    _                <- printLine(s"Using: ${path.toAbsolutePath}")
    (rawRanges, ids) <- parseFrom(path)

    listOfRanges: List[NumericRange.Inclusive[BigInt]] =
      rawRanges.map((start, end) => NumericRange.inclusive[BigInt](start, end, 1))

    // _ = ids.foreach(id => listOfRanges.exists(_.containsTyped(id))) // boom.
    // _ = ids.foreach(id => listOfRanges.exists(_.contains(id)))      // boom.

    // This also explodes.
    _                                                  =
      ids.foreach(id =>
        val fnd = listOfRanges.find(r => r.find(n => n == id).isDefined)
        println(s"$id -> $fnd")
      )

    // This explodes
    _                                                  =
      (BigInt(0) to BigInt(4294967296L)).length
  yield ()
