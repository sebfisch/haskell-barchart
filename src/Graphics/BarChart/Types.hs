{-# LANGUAGE TypeFamilies, FlexibleContexts, NamedFieldPuns, RecordWildCards,
             GeneralizedNewtypeDeriving #-}

module Graphics.BarChart.Types where

import System.IO ( FilePath )
import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Types ( SomeColor(..) )

import Data.Data     ( Data, Typeable )
import Data.List     ( transpose )
import Data.Maybe    ( fromMaybe )
import Control.Arrow ( first )

type Label = String

data BarChart a = BarChart { block_labels :: [Label], bars :: [Bar a] }

data Bar a = Bar { label :: Label, blocks :: [Block a] }

data Block a = Value a | Interval { mean, lower, upper :: a }

class Drawable a
 where
  type Value a

  chart :: Measurable (Value a) => a -> BarChart (Value a) 

class Num a => Measurable a where
  size :: a -> Double

instance Measurable Double  where size = id
instance Measurable Float   where size = realToFrac
instance Measurable Integer where size = fromIntegral
instance Measurable Int     where size = fromIntegral

type ColorName = String

readColor :: ColorName -> SomeColor
readColor color = SomeColor . fromMaybe (error $ "ivalid color: " ++ color) $
                    readColourName color

data Config = Config {
  outFile :: FilePath,
  outputType :: OutputType,
  caption, xLabel, yLabel :: Label,
  barColors :: [SomeColor],
  dimensions :: (Int,Int),
  ratio, fontSize, barRatio :: Double }

conf :: Config
conf = Config { outFile = "", outputType = PNG,
                caption = "", xLabel = "", yLabel = "",
                barColors = map SomeColor [forestgreen,firebrick,midnightblue],
                dimensions = (800,400),
                ratio = 1, fontSize = 12, barRatio = 0.3 }

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

newtype Ratio = Ratio Double
 deriving (Eq,Num,Measurable)

instance Read Ratio where
  readsPrec n = map (first Ratio) . readsPrec n

instance Show Ratio where
  show (Ratio r) = display (100*r) ++ " %"

data MultiBars a = MultiBars [Label] [(Label,[a])]

instance Drawable (MultiBars a)
 where
  type Value (MultiBars a) = a

  chart (MultiBars block_labels pairs) = BarChart{..}
   where bars               = map (uncurry mkBar) pairs
         mkBar label values = Bar{..} where blocks = map Value values

newtype Intervals a = Intervals [(Label,(a,a,a))]

instance Drawable (Intervals a)
 where
  type Value (Intervals a) = a

  chart (Intervals pairs) = BarChart{..}
   where block_labels                   = []
         bars                           = map (uncurry mkBar) pairs
         mkBar label (mean,lower,upper) = Bar{..} where blocks = [Interval{..}]

data MultiBarIntervals a = MBIntervals [Label] [(Label,[(a,a,a)])]

flipMultiBarIntervals :: MultiBarIntervals a -> MultiBarIntervals a
flipMultiBarIntervals (MBIntervals old_block_labels old_bars) =
  MBIntervals new_block_labels new_bars
 where
  new_block_labels = map fst old_bars
  new_bars = zip old_block_labels . transpose . map snd $ old_bars

instance Drawable (MultiBarIntervals a)
 where
  type Value (MultiBarIntervals a) = a

  chart (MBIntervals block_labels pairs) = BarChart{..}
   where bars = map (uncurry mkBar) pairs

         mkBar label ints = Bar{..}
          where blocks = map mkInterval ints
                mkInterval (mean,lower,upper) = Interval{..}
