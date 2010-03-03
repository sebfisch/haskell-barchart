{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts #-}

module Graphics.BarChart.Types where

import System.IO ( FilePath )
import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Types ( SomeColor(..) )

import Control.Arrow ( first )

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
  show (RunTime t) | t >= hours   = display (t/hours)   "h"
                   | t >= minutes = display (t/minutes) "m"
                   | t >= seconds = display (t/seconds) "s"
                   | t >= millis  = display (t/millis)  "ms"
                   | t >= micros  = display (t/micros)  "us"
                   | otherwise    = display (t/nanos)   "ns"
   where hours   = 60 * minutes
         minutes = 60 * seconds
         seconds = 1
         millis  = seconds / 1000
         micros  = millis  / 1000
         nanos   = micros  / 1000

display :: Double -> String -> String
display x s | x >= 100  = show (fromIntegral (round x))             ++ ' ':s
            | x >= 10   = show (fromIntegral (round (10*x)) / 10)   ++ ' ':s
            | otherwise = show (fromIntegral (round (100*x)) / 100) ++ ' ':s

instance Read SomeColor where
  readsPrec _ s = maybe [] (\c -> [(SomeColor c,"")]) (readColourName s)