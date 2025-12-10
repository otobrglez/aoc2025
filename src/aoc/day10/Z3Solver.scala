package aoc.day10

import zio.*
import com.microsoft.z3.*

object Z3Solver:
  private def mkContext: ZIO[Scope, Throwable, Context]    = ZIO.fromAutoCloseable(ZIO.attemptBlocking(new Context()))
  final def withContext[Out](f: Context => Out): Task[Out] = ZIO.scoped(mkContext.map(f))
  def layer: ZLayer[Scope, Throwable, Context]             = ZLayer.fromZIO(mkContext)
