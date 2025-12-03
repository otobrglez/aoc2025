package aoc

import zio.*
import zio.stream.*
import java.nio.file.*

object Main03 extends AOCApp:

  private def computeJoltage(raw: String): Int =
    if raw.length < 2 then return 0
    var (maxNum, maxRight) = Int.MinValue -> raw.last.asDigit
    for i <- raw.length - 2 to 0 by -1 do
      val current = raw(i).asDigit
      val num     = current * 10 + maxRight
      if num > maxNum then maxNum = num
      if current > maxRight then maxRight = current

    maxNum

  private def computeJoltage2(raw: String): BigInt =
    val needed = 12
    if raw.length < needed then return BigInt(0)

    var (currentStart, result) = 0 -> BigInt(0)
    for i <- 0 until needed do
      val remainingNeeded        = needed - 1 - i
      val searchEnd              = raw.length - 1 - remainingNeeded
      var (bestDigit, bestIndex) = -1 -> -1

      var (j, foundMax) = currentStart -> false
      while j <= searchEnd && !foundMax do
        val digit = raw(j).asDigit
        if digit == 9 then
          bestDigit = 9
          bestIndex = j
          foundMax = true
        else if digit > bestDigit then
          bestDigit = digit
          bestIndex = j
        j += 1

      result = result * 10 + bestDigit
      currentStart = bestIndex + 1

    result

  private def sumWith[Out](path: Path, f: String => Out)(implicit ev: Numeric[Out]) =
    ZStream
      .fromJavaIterator(Files.readAllLines(path).iterator())
      .map(_.trim)
      .map(f)
      .runSum

  def program(path: Path) = for
    sum1 <- sumWith(path, computeJoltage).debug("Part 1")
    _     = assert(sum1 == 357)
    sum2 <- sumWith(path, computeJoltage2).debug("Part 2")
    _     = assert(sum2 == BigInt("3121910778619"))
  yield ()
