# 🎄 Advent of Code 2025 in Scala 🎄

[![MIT license](https://img.shields.io/badge/license-MIT-brightgreen.svg)](https://opensource.org/licenses/MIT)

This is my [AoC 2025][aoc] in [Scala 3][scala] with [Scala CLI][scala-cli] and some [ZIO](https://zio.dev/).

## Solutions

- [Day 1](./src/aoc/Main01.scala)
- [Day 2](./src/aoc/Main02.scala)
- [Day 3](./src/aoc/Main03.scala)
- [Day 4](./src/aoc/Main04.scala)
- [Day 5](./src/aoc/Main05.scala) - ["BigInt-ranges are br0ken (example)"](./src/aoc/Main05x.scala)
- [Day 6](./src/aoc/Main06.scala)
- [Day 7](./src/aoc/day7/Main07.scala) - [Scala TailCalls](https://www.scala-lang.org/api/3.x/scala/util/control/TailCalls$.html), [Scala TrieMap](https://www.scala-lang.org/api/3.1.2/scala/collection/concurrent/TrieMap.html), [Trampoline Optimization](https://marmelab.com/blog/2018/02/12/understanding-recursion.html#trampoline-optimization)
- [Day 8](./src/aoc/Main08.scala) - [Scala Named Tuples](https://docs.scala-lang.org/sips/named-tuples.html)
- [Day 9](./src/aoc/day9/Main09.scala) - [Scala parallel collections](https://github.com/scala/scala-parallel-collections)
- [Day 10](./src/aoc/day10/Main10.scala) - [Bitmasking](https://www.baeldung.com/java-bitmasking), [Microsoft Z3](https://www.microsoft.com/en-us/research/project/z3-3/) ([SMT Solvers](https://de-engineer.github.io/SMT-Solvers/), [Satisfiability modulo theories](https://en.wikipedia.org/wiki/Satisfiability_modulo_theories)), [Scala PriorityQueue](https://scala-lang.org/api/3.x/scala/collection/mutable/PriorityQueue$.html)
- [Day 11](./src/aoc/day11/Main11.scala) - [Depth-first search (DFS)](https://en.wikipedia.org/wiki/Depth-first_search), [BitSet](https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/BitSet.html), [DP and DAGs](https://www.geeksforgeeks.org/competitive-programming/dynamic-programming-dp-and-directed-acyclic-graphs-dag/), [Bitmask DP](https://dilipkumar.medium.com/bitmasking-dynamic-programming-coding-pattern-789030c04cd8)
- Day 12

## Development

Please make sure you have [Scala CLI][scala-cli] installed. My preferred way to manage dependencies and have reproducible per-project isolation is to use [devenv](https://devenv.sh/). 
So make sure you have it installed and check the contence of [`devenv.nix`](./devenv.nix).

Check the content of [`justfile`](./justfile) for some useful commands.

```bash
just aoc_run Main04 ./inputs/04-example.txt # For given examples
just aoc_run day10.Main10 ./inputs/10-input.txt # For real input
```

\- [Oto Brglez](https://github.com/otobrglez)

[scala]: https://www.scala-lang.org/

[scala-cli]: https://scala-cli.virtuslab.org/

[aoc]: https://adventofcode.com/
