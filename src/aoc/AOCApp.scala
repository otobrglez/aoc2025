package aoc

import zio.*
import java.nio.file.{Path, Paths}

trait AOCApp extends ZIOAppDefault:

  final protected def firstArgumentAsFile: RIO[ZIOAppArgs, Path] =
    ZIOAppArgs.getArgs
      .flatMap(ch => ZIO.getOrFailWith(new RuntimeException("First argument missing"))(ch.headOption))
      .flatMap(path => ZIO.attempt(Paths.get(path)))

  def program(input: Path): Task[Unit]

  override def run: ZIO[ZIOAppArgs & Scope, Any, Any] = for
    path <- firstArgumentAsFile
    out  <- program(path)
  yield out
