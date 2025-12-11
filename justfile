jvm := "system"
scala := "3.7.4"

aoc_run main_class input_file:
    scala-cli -q -S {{ scala }} --jvm {{ jvm }} src src/aoc/{{ main_class }}.scala \
      --main-class=aoc.{{ main_class }} -- {{ input_file }}
console:
  scala-cli console -S {{ scala }} --jvm {{ jvm }}
