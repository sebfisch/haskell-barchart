barchart is a command-line program for generating bar charts from CSV
files. It has special support for creating charts from data generated
by the Haskell benchmarking tools [criterion] and
[progression]. barchart uses the Haskell [Diagrams] library for
rendering and, hence, depends on a Haskell binding to Cairo which is
part of [Gtk2Hs].

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

## flags

## criterion

## progression

# api

# contribute

The source code is on [GitHub] so you can create or vote on [issues]
to ask for extensions or fork this project to write extensions
yourself.

# contact

For questions or feedback email [Sebastian Fischer][email].

[email]: mailto:sebf@informatik.uni-kiel.de
[GitHub]: http://github.com/sebfisch/haskell-barchart 
[issues]: http://github.com/sebfisch/haskell-barchart/issues

[guitar]: http://sebfisch.github.com/haskell-barchart/examples/guitar.png

[criterion]: http://hackage.haskell.org/package/criterion
[progression]: http://hackage.haskell.org/package/progression
[Diagrams]: http://code.haskell.org/diagrams/
[Gtk2Hs]: http://www.haskell.org/gtk2hs/