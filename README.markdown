barchart is a command-line program for generating bar charts from CSV
files. It has special support for creating charts from data generated
by the Haskell benchmarking tools [criterion] and
[progression]. barchart can create `.png`, `.svg`, `.pdf`, and `.ps`
files using the Haskell [Diagrams] library for rendering and, hence,
depends on a Haskell binding to Cairo which is part of [Gtk2Hs].

# install

# use

Bar charts can be created from CSV files using the `barchart`
command-line utility. For example, if you want to track how many hours
you practice playing the guitar on each day of the week, you can
create a file `guitar.csv` with the following contents:

    Mon,1.2
    Tue,0.3
    Wed,2.1
    Thu,0.9
    Fri,1.1
    Sat,3.2
    Sun,3.1

The call

    # barchart --title="Practice hours" guitar.csv 

creates a file `guitar.png` which looks like this:

![Practice hours][guitar]

Each bar is labeled with a weekday and has an associated practice
hour. The chart is scaled automatically such that the largest bar
spans the (configurable, see below) height of the chart. The `--title`
flag passed to `barchart` in the above call is optional. If you do not
supply one, then barchart uses the basename of the CSV file as title
of the chart.

If you want to track practice hours over multiple weeks, you can
create a file with a _mean_, _minimal_, and _maximal_ values for each
day of the week:

    Mon,1.2,0.9,1.7
    Tue,0.3,0.1,0.5
    Wed,2.1,1.5,2.5
    Thu,0.9,0.4,1.0
    Fri,1.1,1.0,1.2
    Sat,3.2,1.5,5.2
    Sun,3.1,2.3,4.2

Calling `barchart` in the `interval` mode

    # barchart intervals --title="Mean practice hours" guitar-mean.csv

produces the following bar chart:

![Mean practice hours][guitar-mean]

In this chart, each bars represents the mean practice hour for a day
of the week and the minimum and maximum values are depicted with
intervals on the right edge of a bar.

If you want to compare your practice hours for each day of the week
and split it by months, you can create a CSV file like this:

    Mon,1.2,2.1,1.7
    Tue,0.6,0.7,0.8
    Wed,2.1,1.2,2.5
    Thu,0.9,1.5,1.7
    Fri,1.1,1.3,0.7
    Sat,3.2,1.7,4.3
    Sun,3.1,3.2,2.1

We can use `barchart` (in the default mode) 

    # barchart --title="Practice hours per month" --division="Jan Feb Mar" guitar-months.csv

to create the following diagram:

![Practice hours by month][guitar-months]

Each bar is divided into different blocks which all have an associated
amount of practice hours. Green blocks represent practice hours in
January, red blocks in February, and blue blocks represent practice
hours in March. The block labels are given to `barchart` via the
`--division` flag. You can also draw multiple blocks per bar in
`interval` mode but then three values (_mean_,_min_,_max_) are used
for each block. Hence, if you want to depict mean practice times with
deviations for January, February, and March, you must create a CSV
file where each day of the week is followed by nine practice times.

## flags

The `barchart` program can be configured using command-line flags. We
can use the `--help` flag to print a summary:

    # barchart --help
    Bar Chart 0.1
    
    barchart [blocks] [FLAG] [FILE]
    
    barchart intervals [FLAG] [FILE]
    
    barchart criterion [FLAG] [FILE]
    
         --summary               Show benchmark summary (default)
      -s --summary-comparison    Compare different benchmark summaries
      -b --benchmark-comparison  Compare different benchmarks
    
    barchart progression [FLAG] [FILE]
    
      -s --summary-comparison    Breakdown chart by benchmark summary (default)
      -b --benchmark-comparison  Breakdown chart by benchmarks
    
    Common flags:
      -? --help[=FORMAT]         Show usage information (optional format)
      -V --version               Show version information
      -v --verbose               Higher verbosity
      -q --quiet                 Lower verbosity
      -o --out-file=FILE         Name of generated file
      -t --title=STRING          Title of bar chart
      -x --xlabel=STRING         Label of x axis
      -y --ylabel=STRING         Label of y axis
      -g --guess-file-type       Guess output file type by name (default)
         --png                   Generate .png file
         --svg                   Generate .svg file
         --pdf                   Generate .pdf file
         --ps                    Generate .ps file
      -d --division=STRINGS      Labels separated by whitespace
      -c --colors=STRINGS        Color names separated by whitespace
      -w --width=NUM             Width of generated bar chart (default=600)
      -h --height=NUM            Height of generated bar chart (default=300)
      -l --label-size=NUM        Font size used for labels (default=12)
         --bar-width=FLOAT       Bar width between 0 and 1 (default=0.3)

`barchart` can be run in different modes. We have already seen the
default (`blocks`) mode and the `intervals` mode. The `criterion` and
`progression` modes are described below. Most command-line flags are
self explanatory. Apart from what we have seen in the example above,
the following options are particularly interesting:

  * `--xlabel` and `--ylabel` label the axis of the coordinate system.

  * `--colors` change the colors of the different blocks of a bar. You
    can use all color names listed in the [SVG 1.1
    Specification][Colors]. If you specify fewer colors than there are
    blocks, then colors are reused in a cyclic fashion. The default
    value for this argument is `--colors="seagreen firebrick
    midnightblue"`.

  * `--width` and `--height` specify the dimensions of the generated
    _chart_. The generated _picture_ is a little larger because of the
    title and bar labels. If you want to draw a chart with many bars,
    you should increase the width compared to the height or tweak the
    bar width.

  * `--bar-width` is a value between 0.0 and 1.0 hat specifies how
    thick the bars are compared to the bar distance. With a value of
    `1.0` the bars are drawn directly next to each other, a value of
    0.0 would draw bars that are so thin that you cannot see them.

## criterion

[criterion] is a Haskell tool for statistically robust benchmarking
that can generate graphs which, for example, depict the densities of
execution times. criterion can also generate a CSV file summarising
all benchmarks but does not provide means to visualise these
summaries.

We can use `barchart` in `criterion` mode to draw graphs based on the
summary files generated by criterion. To demonstrate the criterion
mode, we write a small Haskell program that benchmarks a simple
definition of the [factorial] function:

~~~ { .Haskell }
import Criterion.Main

main = defaultMain [bgroup "fac" [bench (show n) (nf product [1..n]) | n <- ns]]
 where ns = [10^4 * k | k <- [1,2,3]] :: [Integer]
~~~

We can compile this program, generate a benchmark summary using
criterion, and visualise it using barchart as follows:

    # ghc -O2 --make factorial
    # ./factorial --summary=factorial.csv
    # barchart criterion factorial.csv

These three calls generate a bar chart with one bar for each benchmark
whose size is proportional to the corresponding run time.

![fac]

We can now modify the program to use an explicitly recursive
definition of the fibonacci function to see whether this affects the
run times.

~~~ { .Haskell }
fac 0 = 1
fac n = n * fac (n-1)
~~~

After generating another summary file `factorial2.csv` we could
generate another bar chart to visualise it and view both charts side
by side to compare the run times of the different implementations of
the fibonacci function. However, instead of generating two different
charts we can also generate a single chart that combines information
from multiple benchmark summaries.

    # barchart criterion --summary-comparison factorial.csv factorial2.csv

We can pass as many summary files as we like and barchart will draw a
bar for each summary file with blocks representing the different
benchmarks.

![fac-summaries]

We can see clearly that the original implementation using `product` is
faster than the explicitly recursive definition of the factorial
function.

Instead of drawing different bars for different summaries, barchart
can also draw different bars for the different benchmarks with blocks
for the summaries.

![fac-benchmarks]

This is less useful for comparing different implementations but useful
to compare the different benchmarks using all implementations at once.

## progression

# contribute

The source code is on [GitHub] so you can create or vote on [issues]
to ask for extensions or fork this project to write extensions
yourself.

## limitations

Currently, barchart does not support negative quantities and draws
weird bars if you use them anyway. Also, the legend for blocks is
sometimes drawn suboptimally. I guess I made a mistake but I cannot
find it.

# contact

For questions or feedback email [Sebastian Fischer][email].

[email]: mailto:sebf@informatik.uni-kiel.de
[GitHub]: http://github.com/sebfisch/haskell-barchart 
[issues]: http://github.com/sebfisch/haskell-barchart/issues

[guitar]: http://sebfisch.github.com/haskell-barchart/examples/guitar.png
[guitar-mean]: http://sebfisch.github.com/haskell-barchart/examples/guitar-mean.png
[guitar-months]: http://sebfisch.github.com/haskell-barchart/examples/guitar-months.png
[fac]: http://sebfisch.github.com/haskell-barchart/examples/factorial.png

[criterion]: http://hackage.haskell.org/package/criterion
[progression]: http://hackage.haskell.org/package/progression
[Diagrams]: http://code.haskell.org/diagrams/
[Gtk2Hs]: http://www.haskell.org/gtk2hs/
[Colors]: http://www.w3.org/TR/SVG11/types.html#ColorKeywords
[factorial]: http://en.wikipedia.org/wiki/Factorial