{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Graphics.BarChart.Parser.Criterion where

import Text.CSV

import System.FilePath

import Graphics.BarChart.Types
import Graphics.BarChart.Parser
import Graphics.BarChart.Rendering

criterionChart :: CSV -> BarChart RunTime
criterionChart (_:csv) = intervalChart $ map (take 4) csv

comparisonChart :: Bool -> [(Label,CSV)] -> BarChart RunTime
comparisonChart flip
  = chart
  . (if flip then flipMultiBarIntervals else id)
  . mergeIntervals
  . map (\ (label,_:csv) -> (label, parseIntervals $ map (take 4) csv))

writeCriterionChart :: Config -> FilePath -> IO ()
writeCriterionChart config file =
  renderWith config . criterionChart =<< readCSV file

writeComparisonChart :: Bool -> Config -> [FilePath] -> IO ()
writeComparisonChart flip config@Config{..} files =
  do csvs <- mapM readCSV files
     renderWith config . comparisonChart flip $
       zip (map dropExtension files) csvs
