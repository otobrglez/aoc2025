package aoc.day10

import zio.*
import com.microsoft.z3.*

// solve x > 2 and y < 10 and x + 2y = 7
private def demoEquation = Z3Solver.withContext: ctx =>
  val (x, y) = ctx.mkIntConst("x") -> ctx.mkIntConst("y")

  val solved = ctx.mkSolver()
  solved.add(ctx.mkGt(x, ctx.mkInt(2)))
  solved.add(ctx.mkLt(y, ctx.mkInt(10)))
  solved.add(
    ctx.mkEq(
      ctx.mkAdd(x, ctx.mkMul(y, ctx.mkInt(2))),
      ctx.mkInt(7)
    )
  )

  solved.check() match
    case Status.SATISFIABLE =>
      val model = solved.getModel
      val xVal  = model.evaluate(x, false).asInstanceOf[IntNum].getInt64
      val yVal  = model.evaluate(y, false).asInstanceOf[IntNum].getInt64
      println(s"(example solution) x = $xVal, y = $yVal")
    case status             =>
      println("No solution found: " + status)

private def demoWear = Z3Solver.withContext: ctx =>
  val (tie, shirt) = ctx.mkBoolConst("tie") -> ctx.mkBoolConst("shirt")

  val solver = ctx.mkSolver()
  solver.add(ctx.mkOr(tie, shirt))                       // tie OR shirt
  solver.add(ctx.mkImplies(tie, shirt))                  // tie IMPLIES shirt
  solver.add(ctx.mkOr(ctx.mkNot(tie), ctx.mkNot(shirt))) // NOT tie OR NOT shirt

  solver.check() match
    case Status.SATISFIABLE =>
      val model    = solver.getModel
      val tieVal   = model.eval(tie, true)
      val shirtVal = model.eval(shirt, true)
      s"tie = $tieVal, shirt = $shirtVal"
    case status             =>
      s"No solution: $status"

// Examples from: https://microsoft.github.io/z3guide/programming/Z3%20JavaScript%20Examples
object Z3SandboxApp extends ZIOAppDefault:
  def program = for
    _ <- demoEquation
    _ <- demoWear.debug("Wear")
    _  = println("Done!")
  yield ()

  def run = program
