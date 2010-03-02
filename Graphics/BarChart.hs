{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Graphics.BarChart (

  -- ^ Bar chart datatypes

  Label, BarChart(..), Bar(..), Value(..),

  -- ^ Rendering bar charts

  Config(..), defaultConfig, render, diagram, Measurable(..),

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

data Config = Config {
  filename :: FilePath,
  width, height :: Int,
  padding, ratio, captionSize, labelSize, labelSep, barSep, barWidth :: Double
 }

defaultConfig :: Config
defaultConfig =
  Config { filename = "bar-chart.png",
           width = 400, height = 200,
           padding = 10, ratio = 1, captionSize = 15,
           labelSize = 10, labelSep = 5,
           barSep = 100, barWidth = 20 }

render :: Measurable a => BarChart a -> IO ()
render = renderWithConfig defaultConfig

renderWithConfig :: Measurable a => Config -> BarChart a -> IO ()
renderWithConfig config@Config{..} chart =
  renderAs PNG filename (Width (fromIntegral width)) . diagram config

diagram :: Measurable a => Config -> BarChart a -> Diagram
diagram config@Config{..} chart = draw config{ ratio = hratio / wratio } chart
 where
  wratio = fromIntegral width / barChartWidth config chart
  hratio = fromIntegral height / barChartHeight chart

barChartWidth :: Config -> BarChart a -> Double
barChartWidth Config{..} BarChart{..} = genericLength bars * (barSep + barWidth)

barChartHeight :: Measurable a => BarChart a -> Double
barChartHeight BarChart{..}
  | null bars = 0
  | otherwise = maximum (map barSize bars)

barSize :: Measurable a => Bar a -> Double
barSize Bar{..} = sum (map valueSize values)

valueSize :: Measurable a => Value a -> Double
valueSize (Value x)    = size x
valueSize Interval{..} = size mean

class Drawable a where
  draw :: Config -> a -> Diagram

instance Measurable a => Drawable (BarChart a) where
  draw config@Config{..} chart@BarChart{..} =
    pad padding padding $
      vcatA hcenter
        [text captionSize caption,
         hdistribA (barSep/2) left bottom
           [yaxis, hdistribA barSep left bottom (map (draw config) bars)]
         // xaxis]
   where
    width  = barChartWidth config chart
    height = ratio * barChartHeight chart

    xaxis = vsepA labelSep right [straight (pathFromVectors [(width,0)]),
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
         uplowdiff = ratio * size (upper - lower)

ctext :: Double -> String -> Diagram
ctext size string = translateY (-size/2) (text size string)
