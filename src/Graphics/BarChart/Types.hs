{-# LANGUAGE TypeFamilies, FlexibleContexts, NamedFieldPuns, RecordWildCards,
             GeneralizedNewtypeDeriving #-}

module Graphics.BarChart.Types where

import System.IO ( FilePath )
import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Types ( SomeColor(..) )

import Data.Data     ( Data, Typeable )
import Data.List     ( nub, transpose )
import Data.Maybe    ( fromJust, fromMaybe )
import Control.Arrow ( first )

type Label = String

-- | Bar charts consist of a (possibly empty) list of labels for the
--   diferent blcks of bars and the bars themselves.
-- 
data BarChart a = BarChart {

  -- | Labels of blocks in bars. Drawn as a legend if non-empty.
  block_labels :: [Label],

  -- | The different bars of the chart.
  bars :: [Bar a] }
 deriving Show

-- | Represents one bar of a bar chart.
data Bar a = Bar {

  -- | Label written underneath
  label :: Label,

  -- | Different blocks of the bar. Simple charts contain only one
  --   block per bar.
  blocks :: [Block a] }
 deriving Show

-- | Bocks either have a single associated value or a mean value along
--   with minimum and maximum deviation.
-- 
data Block a = Value a | Interval { mean :: a, lower :: a, upper :: a }
 deriving Show

-- | Instances of this class can be depicted in bar charts.
-- 
class Num a => Measurable a where

  -- | Measures the given value to figure out the correponding height
  --   of the bar.
  -- 
  size :: a -> Double

instance Measurable Double  where size = id
instance Measurable Float   where size = realToFrac
instance Measurable Integer where size = fromIntegral
instance Measurable Int     where size = fromIntegral

type ColorName = String

readColor :: ColorName -> SomeColor
readColor color = SomeColor . fromMaybe (error $ "ivalid color: " ++ color) $
                    readColourName color

-- | Specifies how bar charts are rendered
data Config = Config {
  -- | file to which the bar chart is written
  outFile :: FilePath,

  -- | Type of generated file
  outputType :: OutputType,

  -- | Title of the generated chart
  caption :: Label, 

  -- | Label of the x-axis
  xLabel :: Label, 

  -- | label of the y-axis
  yLabel :: Label,

  -- | Colors for the different blocks of a bar. If there are fewer
  --   colors than blocks, then colors are reused in a cyclic fashion.
  barColors :: [SomeColor],

  -- | Dimensions of the generated chart. The image will be a bit
  --   larger because of additiona space used for labels.
  dimensions :: (Int,Int),

  -- | Scales the height of the chart. The given ratio is multiplied
  --   with the 'size' of bars as given by the corresponding
  --   'Measurable' instance.
  ratio :: Double, 

  -- | Specifies the size of fonts used for labels.
  fontSize :: Double, 

  -- | Value between 0.0 and 1.0 which pecifies the width of
  --   bars. Zero means that the bars are lines and 1.0 means that the
  --   is no space between bars.
  barRatio :: Double }

-- | The default configuration generates a PNG file with a chart of
--   size 600x300 pixels. The output file is left unspecified and you
--   should provide one if you use a cstom configuration.
-- 
conf :: Config
conf = Config { outFile = "", outputType = PNG,
                caption = "", xLabel = "", yLabel = "",
                barColors = map SomeColor [forestgreen,firebrick,midnightblue],
                dimensions = (600,300),
                ratio = 1, fontSize = 12, barRatio = 0.3 }

-- | Wrapper around the 'Double' type used in bar charts for criterion
--   summary files. It has a custom 'Show' instance to produce labels
--   like @10ms@ or @2h@ rather than showing the plain 'Double'
--   values.
-- 
newtype RunTime = RunTime Double
 deriving (Eq,Num,Measurable)

instance Read RunTime where
  readsPrec n = map (first RunTime) . readsPrec n

instance Show RunTime where
  show (RunTime t) | t >= hours   = display (t/hours)   ++ " h"
                   | t >= minutes = display (t/minutes) ++ " m"
                   | t >= seconds = display (t/seconds) ++ " s"
                   | t >= millis  = display (t/millis)  ++ " ms"
                   | t >= micros  = display (t/micros)  ++ " us"
                   | otherwise    = display (t/nanos)   ++ " ns"
   where hours   = 60 * minutes
         minutes = 60 * seconds
         seconds = 1
         millis  = seconds / 1000
         micros  = millis  / 1000
         nanos   = micros  / 1000

display :: Double -> String
display x | x >= 100  = show (fromIntegral (round x))
          | x >= 10   = show (fromIntegral (round (10*x)) / 10)
          | otherwise = show (fromIntegral (round (100*x)) / 100)

-- | Wrapper around the double type used in bar charts for progression
--   summaries. It has a custom 'Show' instance that shows the 'Double'
--   values as percentages.
-- 
newtype Ratio = Ratio Double
 deriving (Eq,Num,Measurable)

instance Read Ratio where
  readsPrec n = map (first Ratio) . readsPrec n

instance Show Ratio where
  show (Ratio r) = display (100*r) ++ " %"

-- | Values of this type are drawn as charts where each bar may
--   consist of multiple blocks.
-- 
data MultiBars a = MultiBars [Label] [(Label,[a])]
 deriving Show

-- | Converts bars with multiple blocks into their 'BarChart'
--   representation.
drawMultiBars :: Measurable a => MultiBars a -> BarChart a
drawMultiBars (MultiBars block_labels pairs) = BarChart{..}
 where bars               = map (uncurry mkBar) pairs
       mkBar label values = Bar{..} where blocks = map Value values

-- | Values of this type are drawn as charts where each bar has an
--   associated deviation depicted as an interval next to the bar.
-- 
newtype Intervals a = Intervals [(Label,(a,a,a))]
 deriving Show

-- | Converts bars with associated deviation into their 'BarChart'
--   representation.
-- 
drawIntervals :: Measurable a => Intervals a -> BarChart a
drawIntervals (Intervals pairs) = BarChart{..}
 where block_labels                   = []
       bars                           = map (uncurry mkBar) pairs
       mkBar label (mean,lower,upper) = Bar{..} where blocks = [Interval{..}]

-- | Values of this type are drawn as charts where each bar may be
--   divided into multiple blocks with an associated deviation
--   depicted as intervals next to them.
-- 
data MultiBarIntervals a = MBIntervals [Label] [(Label,[(a,a,a)])]
 deriving Show

-- | Merges several interval charts into a chart where each bar has
--   multiple blocks that represent the different interval charts.
-- 
mergeIntervals :: Num a => [(Label,Intervals a)] -> MultiBarIntervals a
mergeIntervals xs =
  MBIntervals block_labels [ (label,intervals label) | label <- bar_labels ]
 where
  bar_labels   = map fst xs
  block_labels = nub (concatMap ((\ (Intervals ys) -> map fst ys) . snd) xs)

  intervals l  = map (fromMaybe (0,0,0) . flip lookup ys) block_labels
   where Intervals ys = fromJust (lookup l xs)

-- | Swaps bars and blocks of a chart that contains both and
--   associated deviations.
-- 
flipMultiBarIntervals :: MultiBarIntervals a -> MultiBarIntervals a
flipMultiBarIntervals (MBIntervals old_block_labels old_bars) =
  MBIntervals new_block_labels new_bars
 where
  new_block_labels = map fst old_bars
  new_bars = zip old_block_labels . transpose . map snd $ old_bars

-- | Converts bars with multiple blocks and associated deviations into
--   their 'BarChart' representation.
-- 
drawMultiBarIntervals :: Measurable a => MultiBarIntervals a -> BarChart a
drawMultiBarIntervals (MBIntervals block_labels pairs) = BarChart{..}
 where bars = map (uncurry mkBar) pairs

       mkBar label ints = Bar{..}
        where blocks = map mkInterval ints
              mkInterval (mean,lower,upper) = Interval{..}
