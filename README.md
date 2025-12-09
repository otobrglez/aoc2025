# 🎄 Advent of Code 2025 in Scala 🎄

[![MIT license](https://img.shields.io/badge/license-MIT-brightgreen.svg)](https://opensource.org/licenses/MIT)

This is my [AoC 2025][aoc] in [Scala 3][scala] with [Scala CLI][scala-cli].

## Solutions

- [Day 1](./src/aoc/Main01.scala)
- [Day 2](./src/aoc/Main02.scala)
- [Day 3](./src/aoc/Main03.scala)
- [Day 4](./src/aoc/Main04.scala)
- [Day 5](./src/aoc/Main05.scala)
- [Day 6](./src/aoc/Main06.scala)
- [Day 7](./src/aoc/day7/Main07.scala) - [Scala TailCalls](https://www.scala-lang.org/api/3.x/scala/util/control/TailCalls$.html), [Trampoline Optimization](https://marmelab.com/blog/2018/02/12/understanding-recursion.html#trampoline-optimization)
- [Day 8](./src/aoc/Main08.scala) - [Scala Named Tuples](https://docs.scala-lang.org/sips/named-tuples.html)
- [Day 9](./src/aoc/day9/Main09.scala) - [Scala parallel collections](https://github.com/scala/scala-parallel-collections)
- Day 10
- Day 11
- Day 12

## Development

```bash
just aoc_run Main04 ./inputs/04-example.txt # For given examples
just aoc_run Main04 ./inputs/04-input.txt # For my input
```

\- [Oto Brglez](https://github.com/otobrglez)

[scala]: https://www.scala-lang.org/

[scala-cli]: https://scala-cli.virtuslab.org/

[aoc]: https://adventofcode.com/
