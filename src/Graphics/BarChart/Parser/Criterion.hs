{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Graphics.BarChart.Parser.Criterion where

import Text.CSV

import System.FilePath

import Graphics.BarChart.Types
import Graphics.BarChart.Parser
import Graphics.BarChart.Rendering

criterionChart :: Label -> CSV -> BarChart RunTime
criterionChart name (_:csv) =
  intervalChart name (["benchmark","run time"] : map (take 4) csv)

comparisonChart :: Label -> [(Label,CSV)] -> BarChart RunTime
comparisonChart name
  = chart name "benchmarks" "run times"
  . mergeIntervals
  . map (\ (label,_:csv) -> (label, parseIntervals $ map (take 4) csv))

writeCriterionChart :: Config -> FilePath -> IO ()
writeCriterionChart config file =
  renderWith config . criterionChart (dropExtension file) =<< readCSV file

writeComparisonChart :: Config -> [FilePath] -> IO ()
writeComparisonChart config@Config{..} files =
  do csvs <- mapM (readCSV . flip replaceExtension "csv") files
     renderWith config . comparisonChart (dropExtension file_name) $
       zip (map dropExtension files) csvs
