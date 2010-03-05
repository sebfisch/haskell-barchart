barchart is a command-line program for generating bar charts from CSV
files. It has special support for creating charts from data generated
by the Haskell benchmarking tools [criterion] and
[progression]. barchart uses the Haskell [Diagrams] library for
rendering and, hence, depends on a Haskell binding to Cairo which is
part of [Gtk2Hs].

# install

# use

Bar charts can be created from CSV files using the `barchart`
command-line utility. We can create a chart of the top five most
popular Haskell packages (as of march 2009, I did not find more recent
data) by creating a file `top5.csv` with the following contents:

    xmonad,35428
    HTTP,26203
    zlib,24431
    Cabal,23691
    X11,21563

The call

    # barchart --title="Top 5 Haskell Packages" top5.csv 

creates a file `top5.png` which looks like this:

![Top 5 Haskell Packages][top5]

Each bar is labeled with a Haskell package and has an associated
number of downloads. The chart is scaled automatically such that the
largest bar spans the height of the chart. The dimensions of the chart
can be configured using command-line flags. See below for a
description of supported flags.

The `--title` flag passed to `barchart` in the above call is
optional. If you do not supply one, then barchart uses the basename of
the CSV file as title of the chart.

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

[top5]: http://sebfisch.github.com/haskell-barchart/examples/top5.png

[criterion]: http://hackage.haskell.org/package/criterion
[progression]: http://hackage.haskell.org/package/progression
[Diagrams]: http://code.haskell.org/diagrams/
[Gtk2Hs]: http://www.haskell.org/gtk2hs/