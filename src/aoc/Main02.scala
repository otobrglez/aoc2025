package aoc

import zio.*
import zio.stream.*

import java.nio.file.{Files, Path}
import scala.util.Try

object Main02 extends AOCApp:
  private type ID              = BigInt
  private type IDRange         = (ID, ID)
  private type NumberValidator = ID => Boolean

  final private case class LeadingZeroError(raw: String)              extends Throwable:
    override def getMessage: String = s"Leading zeroes are not allowed: $raw"
  final private case class RepeatedNumbersError(invalidIDs: List[ID]) extends Throwable:
    override def getMessage: String = s"Invalid IDs: ${invalidIDs.mkString(",")}"
  final private case class InvalidNumber(raw: String)                 extends Throwable:
    override def getMessage: String = s"Invalid number/range/input: $raw"

  private def parseRange(numberValidator: NumberValidator)(raw: String): Either[Throwable, IDRange] = for
    List(rawStart, rawEnd) <- Try(raw.split("-", 2).map(_.trim).toList).toEither.left.map(_ => InvalidNumber(raw))
    startID                <- Either.cond(!(rawStart.startsWith("0") && rawStart.length > 1), rawStart, LeadingZeroError(rawStart))
    endID                  <- Either.cond(!(rawEnd.startsWith("0") && rawEnd.length > 1), rawEnd, LeadingZeroError(rawEnd))
    bigStartID             <- Try(BigInt(startID)).toEither.left.map(_ => InvalidNumber(startID))
    bigEndID               <- Try(BigInt(endID)).toEither.left.map(_ => InvalidNumber(endID))
    invalidIDs              = (bigStartID to bigEndID).filter(numberValidator).toList
    _                      <- Either.cond(invalidIDs.isEmpty, (), RepeatedNumbersError(invalidIDs))
  yield bigStartID -> bigEndID

  private val isInvalid: NumberValidator = id =>
    val (s, length) = id.toString -> id.toString.length
    length % 2 == 0 && s.take(length / 2) == s.drop(length / 2)

  private val isInvalidPart2: NumberValidator = id =>
    val (s, length) = id.toString -> id.toString.length
    (1 to length / 2).exists(len => length % len == 0 && s == s.take(len) * (length / len))

  private def sumInvalid(path: Path, validator: NumberValidator) =
    ZStream
      .fromJavaIterator(Files.readAllLines(path).iterator())
      .flatMap(line => ZStream.fromIterable(line.split(",")))
      .map(parseRange(validator))
      .collect { case Left(RepeatedNumbersError(invalidIDs)) => invalidIDs }
      .flatMap(ZStream.fromIterable)
      .runSum

  def program(path: Path): Task[Unit] = for
    _ <- sumInvalid(path, isInvalid).debug(s"Part 1")
    _ <- sumInvalid(path, isInvalidPart2).debug(s"Part 2")
  yield ()
