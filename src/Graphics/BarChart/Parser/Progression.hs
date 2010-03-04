{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Graphics.BarChart.Parser.Progression where

import Text.CSV

import System.FilePath

import Graphics.BarChart.Types
import Graphics.BarChart.Parser
import Graphics.BarChart.Rendering

progressionChart :: Bool -> [Label] -> CSV -> BarChart Ratio
progressionChart flip labels csv
  = chart
  . (if flip then flipMultiBarIntervals else id)
  . parseMultiBarIntervals block_labels
  $ csv
 where block_labels | null labels = replicate (length csv) ""
                    | otherwise   = labels

writeProgressionChart :: Bool -> Config -> FilePath -> [Label] -> IO ()
writeProgressionChart flip config@Config{..} file block_labels =
  do csv <- readCSV file
     let chart = progressionChart flip block_labels csv
     renderWith config chart
