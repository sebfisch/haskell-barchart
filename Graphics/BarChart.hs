{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Graphics.BarChart (

  -- ^ Bar chart datatypes

  Label, BarChart(..), Bar(..), Value(..),

  -- ^ Rendering bar charts

  render, Config(..), defaultConfig, Drawable(..), Measurable(..),

  ) where

import System.IO ( FilePath )
import Data.List ( genericLength )

import Graphics.Rendering.Diagrams

type Label = String

data BarChart a = BarChart { caption, xlabel, ylabel :: Label,
                             bars :: [Bar a] }

data Bar a = Bar { label :: Label, values :: [Value a] }

data Value a = Value a | Interval { mean, lower, upper :: a }

class Num a => Measurable a where
  size :: a -> Double

instance Measurable Double where
  size = id

barSize :: Measurable a => Bar a -> Double
barSize Bar{..} = sum (map valueSize values)

valueSize :: Measurable a => Value a -> Double
valueSize (Value x)    = size x
valueSize Interval{..} = size mean

render :: Drawable a => FilePath -> a -> IO ()
render filename = renderAs PNG filename (Width 400) . draw defaultConfig

data Config = Config { padding, ratio, 
                       labelSize, labelSep,
                       barSep, barWidth :: Double }

defaultConfig :: Config
defaultConfig =
  Config { padding = 10, ratio = 1, 
           labelSize = 10, labelSep = 5,
           barSep = 100, barWidth = 20 }

class Drawable a where
  draw :: Config -> a -> Diagram

instance Measurable a => Drawable (BarChart a) where
  draw config@Config{..} BarChart{..} =
    pad padding padding $
      vcatA hcenter
        [text (1.5*labelSize) caption,
         hdistribA (barSep/2) left bottom
           [yaxis, hdistribA barSep left bottom (map (draw config) bars)]
         // xaxis]
   where
    width  = genericLength bars * (barSep + barWidth)
    height | null bars = 0
           | otherwise = maximum (map barSize bars)

    xaxis = hsep labelSep [straight (pathFromVectors [(width,0)]),
                           text labelSize xlabel]
    yaxis = vsep labelSep [text labelSize ylabel,
                           straight (pathFromVectors [(0,height)])]

instance Measurable a => Drawable (Bar a) where
  draw config@Config{..} Bar{..} =
    vcat (reverse (map (draw config) values))

instance Measurable a => Drawable (Value a) where
  draw Config{..} (Value x) =
    hsep labelSep [rect barWidth (ratio * size x), ctext labelSize (show x)]

  draw Config{..} Interval{..} =
    hsep labelSep [rect barWidth (ratio * size mean),
                   deviation,
                   ctext labelSize (show mean)]
   where deviation = translateY (-uplowdiff/2) interval
         interval  = vcat [bound,
                           straight (pathFromVectors [(0,uplowdiff)]),
                           bound]
         bound     = translateX (-labelSep/2)
                       (straight (pathFromVectors [(labelSep,0)]))
         uplowdiff = size (upper - lower)

ctext :: Double -> String -> Diagram
ctext size string = translateY (-size/2) (text size string)
