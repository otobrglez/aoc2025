jvm := "25"

test: aoc_01 aoc_02

aoc_run main_class input_file:
    scala-cli -q --jvm {{ jvm }} src src/aoc/{{ main_class }}.scala \
      --main-class=aoc.{{ main_class }} -- {{ input_file }}

console:
  scala-cli console --jvm {{ jvm }}

aoc_01 input_file="./inputs/01-example.txt":
    scala-cli --jvm {{ jvm }} src src/aoc/Main01.scala \
      --main-class=aoc.Main01 -- {{ input_file }}

aoc_01_real:
    just aoc_01 "./inputs/01-input.txt"

aoc_02 input_file="./inputs/02-example.txt":
    scala-cli --jvm {{ jvm }} src src/aoc/Main02.scala \
      --main-class=aoc.Main02 -- {{ input_file }}

aoc_02_real:
    just aoc_02 "./inputs/02-input.txt"

aoc_run main_class input_file:
    scala-cli -q --jvm {{ jvm }} src src/aoc/{{ main_class }}.scala \
      --main-class=aoc.{{ main_class }} -- {{ input_file }}

aoc_03:
    just aoc_run Main03 ./inputs/03-example.txt

aoc_03_real:
    just aoc_run Main03 ./inputs/03-input.txt


