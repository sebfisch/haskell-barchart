{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Graphics.BarChart.Types where

import System.IO ( FilePath )
import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Types ( SomeColor(..) )

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

instance Measurable Double where
  size = id

data Config = Config {
  filename   :: FilePath,
  bar_colors :: [SomeColor],
  chart_size :: (Int,Int),
  ratio, font_size, bar_ratio :: Double
 }

conf :: Config
conf = Config { filename   = "bar-chart.png",
                bar_colors = cycle (map SomeColor
                                    [midnightblue, firebrick, forestgreen]),
                chart_size = (600,300),
                ratio = 1, font_size = 15, bar_ratio = 0.2 }
