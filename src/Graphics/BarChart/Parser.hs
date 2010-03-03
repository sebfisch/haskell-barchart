module Graphics.BarChart.Parser where

import Text.CSV

import Data.List ( nub )
import Data.Maybe ( fromJust, fromMaybe )

import System.FilePath

import Graphics.BarChart.Types
import Graphics.BarChart.Rendering

parseMultiBars :: Read a => [Label] -> CSV -> MultiBars a
parseMultiBars block_labels = MultiBars block_labels . map parseRecord
 where parseRecord (label:values) = (label,map read values)

parseIntervals :: Read a => CSV -> Intervals a
parseIntervals = Intervals . map parseRecord
 where parseRecord [label,m,l,u] = (label,(read m, read l, read u))

parseMultiBarIntervals :: Read a => [Label] -> CSV -> MultiBarIntervals a
parseMultiBarIntervals block_labels =
  MBIntervals block_labels . map parseRecord
 where parseRecord (label:fields) = (label, triples fields)
       triples (x:y:z:xs) = (read x,read y,read z) : triples xs
       triples _          = []

mergeIntervals :: Num a => [(Label,Intervals a)] -> MultiBarIntervals a
mergeIntervals xs =
  MBIntervals block_labels [ (label,intervals label) | label <- bar_labels ]
 where
  bar_labels   = map fst xs
  block_labels = nub (concatMap ((\ (Intervals ys) -> map fst ys) . snd) xs)

  intervals l  = map (fromMaybe (0,0,0) . flip lookup ys) block_labels
   where Intervals ys = fromJust (lookup l xs)

multiBarChart :: (Measurable a, Read a) => [Label] -> CSV -> BarChart a
multiBarChart block_labels = chart . parseMultiBars block_labels

intervalChart :: (Measurable a, Read a) => CSV -> BarChart a
intervalChart = chart . parseIntervals

multiBarIntervalChart :: (Measurable a, Read a) => [Label] -> CSV -> BarChart a
multiBarIntervalChart block_labels =
  chart . parseMultiBarIntervals block_labels

writeMultiBarChart :: Config -> FilePath -> [Label] -> IO ()
writeMultiBarChart config file block_labels =
  do csv <- readCSV file
     let chart = multiBarChart block_labels csv :: BarChart Double
     renderWith config chart

writeIntervalChart :: Config -> FilePath -> IO ()
writeIntervalChart config file =
  do csv <- readCSV file
     let chart = intervalChart csv :: BarChart Double
     renderWith config chart

writeMultiBarIntervalChart :: Config -> FilePath -> [Label] -> IO ()
writeMultiBarIntervalChart config file block_labels =
  do csv <- readCSV file
     let chart = multiBarIntervalChart block_labels csv :: BarChart Double
     renderWith config chart

readCSV :: FilePath -> IO CSV
readCSV file = either (fail . show) (return . clean) =<< parseCSVFromFile file
 where clean = filter (not . all null)
