package aoc.day10

import com.microsoft.z3.*
import zio.*

// solve x > 2 and y < 10 and x + 2y = 7
private def demoInteger = Z3Solver.withContext: ctx =>
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

object Z3SandboxApp extends ZIOAppDefault:
  def program = for
    _ <- demoInteger.debug("Demo integer:")
    _  = println("Done!")
  yield ()

  def run = program
