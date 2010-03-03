{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Graphics.BarChart.Parser.Progression where

import Text.CSV

import System.FilePath

import Graphics.BarChart.Types
import Graphics.BarChart.Parser
import Graphics.BarChart.Rendering

progressionChart :: Label -> CSV -> BarChart Ratio
progressionChart name csv
  = chart name "benchmarks" "run time ratios"
  . flipMultiBarIntervals
  . parseMultiBarIntervals (replicate (length csv) "")
  $ csv

writeProgressionChart :: Config -> FilePath -> IO ()
writeProgressionChart config@Config{..} file =
  do csv <- readCSV file
     let chart = progressionChart (dropExtension file_name) csv
     renderWith config chart
