jvm := "25"

test: aoc_01

aoc_01 input_file="./inputs/01-example.txt":
  scala-cli --jvm {{ jvm }} src src/aoc/Main01.scala -- {{ input_file }} 

aoc_01_real:
  just aoc_01 "./inputs/01-input.txt"

