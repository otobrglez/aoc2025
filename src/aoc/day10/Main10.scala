package aoc.day10

import zio.*
import aoc.AOCApp
import aoc.Ops.*
import java.nio.file.Path
import scala.collection.mutable
import scala.util.Try
import com.microsoft.z3.*

final case class Machine private (
  lights: Vector[Char],
  buttons: Vector[Vector[Int]],
  joltage: Vector[Int]
):
  val buttonsMasks: Vector[Int] = buttons.map(_.foldLeft(0)((mask, i) => mask | (1 << i)))
  val lightsMask: Int           = lights.zipWithIndex.foldLeft(0):
    case mask -> ('#', i) => mask | (1 << i)
    case mask -> _        => mask

object Machine:
  private def parseLights(line: String): Try[Vector[Char]] = Try:
    """\[(.*)]""".r.findFirstMatchIn(line).get.group(1).toVector

  private def parseButtons(line: String): Try[Vector[Vector[Int]]] = Try:
    """]([^{]*)\{""".r
      .findFirstMatchIn(line)
      .map: m =>
        """\(([^)]+)\)""".r
          .findAllMatchIn(m.group(1))
          .map(_.group(1).split(",").map(_.trim.toInt).toVector)
          .toVector
      .getOrElse(Vector.empty)

  private def parseJoltage(line: String): Try[Vector[Int]] = Try:
    """\{([^}]+)\}""".r
      .findFirstMatchIn(line)
      .map(m => """(\d+)""".r.findAllMatchIn(m.group(1)).map(_.group(1).toInt).toVector)
      .getOrElse(Vector.empty)

  def from(line: String): Try[Machine] =
    for lights <- parseLights(line); schema <- parseButtons(line); joltage <- parseJoltage(line)
    yield Machine(lights, schema, joltage)

private def parseMachines(path: Path): Task[Vector[Machine]] =
  path.linesStream.mapZIO(line => ZIO.from(Machine.from(line))).runCollect.map(_.toVector)

private def solvePart1(machine: Machine): Int =
  val target = machine.lightsMask
  val masks  = machine.buttonsMasks
  val start  = 0 // all lights off
  val queue  = mutable.Queue(start -> 0)
  val seen   = mutable.Set(start)

  while queue.nonEmpty do
    val (state, steps) = queue.dequeue()
    if state == target then return steps
    for mask <- masks do
      val nextState = state ^ mask
      if !seen.contains(nextState) then
        seen.add(nextState)
        queue.enqueue(nextState -> (steps + 1))

  throw new IllegalStateException("No solution found")

private def solvePart2x(machine: Machine): Task[Long] = Z3Solver.withContext: ctx =>
  val opt = ctx.mkOptimize()

  // Button (times to press)
  val buttonVars = machine.buttons.indices.map(i => ctx.mkIntConst(s"button-$i")).toArray

  // Circuit index to buttons
  val circuitToButtons: Map[Int, Vector[IntExpr]] =
    machine.buttons.zipWithIndex.foldLeft(Map.empty[Int, Vector[IntExpr]]):
      case acc -> (button, i) =>
        button.foldLeft(acc): (innerAcc, circuit) =>
          innerAcc.updatedWith(circuit):
            case Some(existing) => Some(existing :+ buttonVars(i))
            case None           => Some(Vector(buttonVars(i)))

  // Constraint: sum of presses equals joltage requirement
  circuitToButtons.foreach: (circuitIndex, buttons) =>
    val targetValue        = ctx.mkInt(machine.joltage(circuitIndex))
    val sumOfButtonPresses = ctx.mkAdd(buttons*)
    opt.Add(ctx.mkEq(targetValue, sumOfButtonPresses))

  // Constraint: all button variables must be non-negative
  val zero = ctx.mkInt(0)
  buttonVars.foreach(v => opt.Add(ctx.mkGe(v, zero)))

  // Objective: minimize total button presses
  val totalPresses = ctx.mkIntConst("presses")
  opt.Add(ctx.mkEq(totalPresses, ctx.mkAdd(buttonVars*)))
  opt.MkMinimize(totalPresses)

  // Debugging:
  // println(opt.toString)

  // Solve
  opt.Check() match
    case Status.SATISFIABLE => opt.getModel.eval(totalPresses, false).asInstanceOf[IntNum].getInt64
    case status             => throw new IllegalStateException(s"Could not determine or find solution: $status")

private def part1(machines: Vector[Machine]): Long       = machines.foldLeft(0L)(_ + solvePart1(_))
private def part2(machines: Vector[Machine]): Task[Long] = ZIO.foreachPar(machines)(solvePart2x).map(_.sum)

object Main10 extends AOCApp:

  def program(path: Path) = for
    machines <- parseMachines(path).debugWith(d => s"Loaded: ${d.size}")
    result1   = part1(machines)
    _         = println(s"Part 1: $result1")
    _        <- part2(machines).debugWith(n => s"Part 2: $n")
  yield ()
