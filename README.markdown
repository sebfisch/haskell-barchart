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
create a file with a *mean*, *minimal*, and *maximal* values for each
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

## flags

## criterion

## progression

# api

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

[criterion]: http://hackage.haskell.org/package/criterion
[progression]: http://hackage.haskell.org/package/progression
[Diagrams]: http://code.haskell.org/diagrams/
[Gtk2Hs]: http://www.haskell.org/gtk2hs/