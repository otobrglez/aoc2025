package aoc

import zio.stream.{ZPipeline, ZStream}

import java.nio.file.Path
import zio.*

object Ops:

  extension (p: Path)
    def linesStream: ZStream[Any, Throwable, String] =
      ZStream.fromFile(p.toFile).via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)

  extension [R, E, A](zio: ZIO[R, E, A])
    def debugWith[X <: String](f: A => X): ZIO[R, E, A] = zio.tap(value => ZIO.debug(f(value)))
