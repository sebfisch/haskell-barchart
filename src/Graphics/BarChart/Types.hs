{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts, 
             NamedFieldPuns, RecordWildCards #-}

module Graphics.BarChart.Types where

import System.IO ( FilePath )
import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Types ( SomeColor(..) )

import Data.List     ( transpose )
import Control.Arrow ( first )

instance Read SomeColor where
  readsPrec _ s = maybe [] (\c -> [(SomeColor c,"")]) (readColourName s)

type Label = String

data BarChart a = BarChart { caption, xlabel, ylabel :: Label, 
                             block_labels :: [Label], bars :: [Bar a] }

data Bar a = Bar { label :: Label, blocks :: [Block a] }

data Block a = Value a | Interval { mean, lower, upper :: a }

class Drawable a
 where
  type Value a

  chart :: Measurable (Value a)
        => Label -> Label -> Label -> a -> BarChart (Value a) 

class Num a => Measurable a where
  size :: a -> Double

instance Measurable Double  where size = id
instance Measurable Float   where size = realToFrac
instance Measurable Integer where size = fromIntegral
instance Measurable Int     where size = fromIntegral

data Config = Config {
  file_name  :: FilePath,
  bar_colors :: [SomeColor],
  chart_size :: (Int,Int),
  ratio, font_size, bar_ratio :: Double
 }

conf :: Config
conf = Config { file_name  = "bar-chart.png",
                bar_colors = cycle (map SomeColor
                                    [forestgreen, firebrick, midnightblue]),
                chart_size = (800,400),
                ratio = 1, font_size = 12, bar_ratio = 0.3 }

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

newtype MultiBars a = MultiBars [(Label,[a])]

instance Drawable (MultiBars a)
 where
  type Value (MultiBars a) = a

  chart caption xlabel ylabel (MultiBars pairs) = BarChart{..}
   where block_labels       = []
         bars               = map (uncurry mkBar) pairs
         mkBar label values = Bar{..} where blocks = map Value values

newtype Intervals a = Intervals [(Label,(a,a,a))]

instance Drawable (Intervals a)
 where
  type Value (Intervals a) = a

  chart caption xlabel ylabel (Intervals pairs) = BarChart{..}
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

  chart caption xlabel ylabel (MBIntervals block_labels pairs) = BarChart{..}
   where bars = map (uncurry mkBar) pairs

         mkBar label ints = Bar{..}
          where blocks = map mkInterval ints
                mkInterval (mean,lower,upper) = Interval{..}
