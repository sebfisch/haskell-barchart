{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Graphics.BarChart.Parser.Criterion where

import Text.CSV

import System.FilePath

import Graphics.BarChart.Types
import Graphics.BarChart.Parser
import Graphics.BarChart.Rendering

criterionChart :: CSV -> BarChart RunTime
criterionChart (_:csv) = intervalChart $ map (take 4) csv

comparisonChart :: [(Label,CSV)] -> BarChart RunTime
comparisonChart
  = chart
  . mergeIntervals
  . map (\ (label,_:csv) -> (label, parseIntervals $ map (take 4) csv))

writeCriterionChart :: Config -> FilePath -> IO ()
writeCriterionChart config file =
  renderWith config . criterionChart =<< readCSV file

writeComparisonChart :: Config -> [FilePath] -> IO ()
writeComparisonChart config@Config{..} files =
  do csvs <- mapM readCSV files
     renderWith config . comparisonChart $ zip (map dropExtension files) csvs
