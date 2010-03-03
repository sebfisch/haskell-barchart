{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Graphics.BarChart.Parser.Progression where

import Text.CSV

import System.FilePath

import Graphics.BarChart.Types
import Graphics.BarChart.Parser
import Graphics.BarChart.Rendering

progressionChart :: [Label] -> CSV -> BarChart Ratio
progressionChart labels csv
  = chart
  . flipMultiBarIntervals
  . parseMultiBarIntervals block_labels
  $ csv
 where block_labels | null labels = replicate (length csv) ""
                    | otherwise   = labels

writeProgressionChart :: Config -> FilePath -> [Label] -> IO ()
writeProgressionChart config@Config{..} file block_labels =
  do csv <- readCSV file
     let chart = progressionChart block_labels csv
     renderWith config chart
