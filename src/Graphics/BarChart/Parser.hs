module Graphics.BarChart.Parser where

import Text.CSV

import Data.List ( nub )
import Data.Maybe ( fromJust, fromMaybe )

import System.FilePath

import Graphics.BarChart.Types
import Graphics.BarChart.Rendering

parseMultiBars :: Read a => CSV -> MultiBars a
parseMultiBars = MultiBars . map parseRecord
 where parseRecord (label:values) = (label,map read values)

parseIntervals :: Read a => CSV -> Intervals a
parseIntervals = Intervals . map parseRecord
 where parseRecord [label,m,l,u] = (label,(read m, read l, read u))

parseMultiBarIntervals :: Read a => [Label] -> CSV -> MultiBarIntervals a
parseMultiBarIntervals block_labels = MBIntervals block_labels . map parseRecord
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

multiBarChart :: Label -> CSV -> BarChart Double
multiBarChart name ((xlabel:ylabel:_):csv) =
  chart name xlabel ylabel . parseMultiBars $ csv

intervalChart :: (Measurable a, Read a) => Label -> CSV -> BarChart a
intervalChart name ((xlabel:ylabel:_):csv) =
  chart name xlabel ylabel . parseIntervals $ csv

multiBarIntervalChart :: (Measurable a, Read a)
                      => Label -> [Label] -> CSV -> BarChart a
multiBarIntervalChart name block_labels ((xlabel:ylabel:_):csv) =
  chart name xlabel ylabel . parseMultiBarIntervals block_labels $ csv

writeMultiBarChart :: Config -> FilePath -> IO ()
writeMultiBarChart config file =
  renderWith config . multiBarChart (dropExtension file) =<< readCSV file

writeIntervalChart :: Config -> FilePath -> Label -> IO ()
writeIntervalChart config file name =
  do csv <- readCSV file
     let chart = intervalChart name csv :: BarChart Double
     renderWith config chart

writeMultiBarIntervalChart :: Config -> FilePath -> Label -> [Label] -> IO ()
writeMultiBarIntervalChart config file name block_labels =
  do csv <- readCSV file
     let chart = multiBarIntervalChart name block_labels csv :: BarChart Double
     renderWith config chart

readCSV :: FilePath -> IO CSV
readCSV file = either (fail . show) (return . clean) =<< parseCSVFromFile file
 where clean = filter (not . all null)
