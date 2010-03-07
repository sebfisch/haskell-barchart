module Graphics.BarChart.Parser where

import Text.CSV

import Data.List ( nub )
import Data.Maybe ( fromJust, fromMaybe )

import System.FilePath

import Graphics.BarChart.Types
import Graphics.BarChart.Rendering

-- | Converts a CSV file to be drawn as a chart where each bar may
--   consist of multiple blocks.
-- 
parseMultiBars :: Read a => [Label] -> CSV -> MultiBars a
parseMultiBars block_labels = MultiBars block_labels . map parseRecord
 where parseRecord (label:values) = (label,map read values)

-- | Converts a CSV file to be drawn as a chart where each bar has an
--   attached deviation depicted as an interval next to the bar.
-- 
parseIntervals :: Read a => CSV -> Intervals a
parseIntervals = Intervals . map parseRecord
 where parseRecord [label,m,l,u] = (label,(read m, read l, read u))

-- | Converts a CSV file to be drawn as a chart where each bar may
--   consist of multiple blocks which have an attached deviation
--   depicted as an interval next to them.
-- 
parseMultiBarIntervals :: Read a => [Label] -> CSV -> MultiBarIntervals a
parseMultiBarIntervals block_labels =
  MBIntervals block_labels . map parseRecord
 where parseRecord (label:fields) = (label, triples fields)
       triples (x:y:z:xs) = (read x,read y,read z) : triples xs
       triples _          = []

-- | Used by 'writeMultiBarChart' to create a 'BarChart' from a CSV file.
-- 
multiBarChart :: (Measurable a, Read a)
              => [Label]    -- ^ legend for blocks
              -> CSV        -- ^ comma separated values to depict
              -> BarChart a
multiBarChart block_labels = drawMultiBars . parseMultiBars block_labels

-- | Used by 'writeIntervalChart' to create a 'BarChart' from a CSV file.
-- 
intervalChart :: (Measurable a, Read a)
              => CSV        -- ^ comma separated values to depict 
              -> BarChart a
intervalChart = drawIntervals . parseIntervals

-- | Used by 'writeMultiBarIntervalChart' to create a 'BarChart' from
-- | a CSV file.
-- 
multiBarIntervalChart :: (Measurable a, Read a)
                      => [Label]    -- ^ legend for blocks
                      -> CSV        -- ^ comma separated values to depict
                      -> BarChart a
multiBarIntervalChart block_labels =
  drawMultiBarIntervals . parseMultiBarIntervals block_labels

-- | The first column of the CSV file is parsed as names of the
--   bars. The height of each bar corresponds to the sum of all
--   subsequent entries. If there is more than one entry, the bars are
--   split into blocks.
-- 
writeMultiBarChart :: Config   -- ^ where and how to draw the bar chart
                   -> FilePath -- ^ CSV file to read
                   -> [Label]  -- ^ if non-empty, used as legend for blocks
                   -> IO ()
writeMultiBarChart config file block_labels =
  do csv <- readCSV file
     let chart = multiBarChart block_labels csv :: BarChart Double
     renderWith config chart

-- | The first column of the CSV file is parsed as names of the
--   bars. Three entries following each bar name are parsed as mean,
--   minimum, and maximum value and depicted using an interval next to
--   the bar.
-- 
writeIntervalChart :: Config   -- ^ where and how to draw the bar chart
                   -> FilePath -- ^ CSV file to read
                   -> IO ()
writeIntervalChart config file =
  do csv <- readCSV file
     let chart = intervalChart csv :: BarChart Double
     renderWith config chart

-- | The first column of the CSV file is parsed as names of the
--   bars. The entries following each bar name are parsed as triples
--   of mean, minimum, and maximum value and depicted using an
--   interval next to the bar. The number of subsequent entries must
--   be a multiple of three and each bar is divided into a
--   corresponding number of blocks.
-- 
writeMultiBarIntervalChart :: Config   -- ^ where and how to draw the bar chart
                           -> FilePath -- ^ CSV file to read
                           -> [Label]  -- ^ legend for blocks
                           -> IO ()
writeMultiBarIntervalChart config file block_labels =
  do csv <- readCSV file
     let chart = multiBarIntervalChart block_labels csv :: BarChart Double
     renderWith config chart

readCSV :: FilePath -> IO CSV
readCSV file = either (fail . show) (return . clean) =<< parseCSVFromFile file
 where clean = filter (not . all null)
