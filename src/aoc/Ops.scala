package aoc

import zio.stream.{ZPipeline, ZStream}
import java.nio.file.Path

object Ops:

  extension (p: Path) def linesStream: ZStream[Any, Throwable, String] =
    ZStream.fromFile(p.toFile).via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
