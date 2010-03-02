{-# LANGUAGE NamedFieldPuns, RecordWildCards, FlexibleInstances #-}

module Graphics.BarChart (

  -- ^ Bar chart datatypes

  Label, BarChart(..), Bar(..), Block(..),

  -- ^ Rendering bar charts

  Config(..), defaultConfig, render, diagram, Measurable(..),

  ) where

import System.IO ( FilePath )
import Data.List ( genericLength )

import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Types ( SomeColor(..) )

type Label = String

data BarChart a = BarChart { caption, xlabel, ylabel :: Label, 
                             blockLabels :: [Label], bars :: [Bar a] }

data Bar a = Bar { label :: Label, blocks :: [Block a] }

data Block a = Value a | Interval { mean, lower, upper :: a }

class Num a => Measurable a where
  size :: a -> Double

instance Measurable Double where
  size = id

data Config = Config {
  filename :: FilePath,
  barColors :: [SomeColor],
  width, height :: Int,
  ratio, padding, captionSize, labelSize, labelSep, barSep, barWidth :: Double
 }

defaultConfig :: Config
defaultConfig =
  Config { filename = "bar-chart.png",
           barColors = cycle (map SomeColor
            [midnightblue, firebrick, forestgreen]),
           width = 600, height = 300, ratio = 1, padding = 10, captionSize = 12,
           labelSize = 8, labelSep = 5, barSep = 100, barWidth = 20 }

render :: Measurable a => BarChart a -> IO ()
render = renderWithConfig defaultConfig

renderWithConfig :: Measurable a => Config -> BarChart a -> IO ()
renderWithConfig config@Config{..} chart =
  renderAs PNG filename (Width (fromIntegral width)) . diagram config $ chart

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
barSize Bar{..} | null blocks = 0
                | otherwise   = sum (upperSize b:map valueSize bs)
 where (b:bs) = reverse blocks

valueSize :: Measurable a => Block a -> Double
valueSize (Value x)    = size x
valueSize Interval{..} = size mean

upperSize :: Measurable a => Block a -> Double
upperSize (Value x)    = size x
upperSize Interval{..} = size upper

class Drawable a where
  draw :: Config -> a -> Diagram

instance Measurable a => Drawable (BarChart a) where
  draw config@Config{..} chart@BarChart{..} =
    pad padding padding $
      vsepA labelSep hcenter
        [text captionSize caption,
         hsep labelSep (map (draw config) (zip barColors blockLabels)),
         vcat [hdistribA (barSep/2) left bottom
                [yaxis, hdistribA barSep left bottom (map (draw config) bars)],
              xaxis,
              hdistribA (barSep/2) left bottom
                [nil, hdistribA barSep left bottom
                        (map (drawBarLabel config) bars)]]]
   where
    width  = barChartWidth config chart
    height = ratio * barChartHeight chart

    xaxis = vsepA labelSep right [straight (pathFromVectors [(width,0)]),
                                  text labelSize xlabel]
    yaxis = vsep labelSep [text labelSize ylabel,
                           straight (pathFromVectors [(0,height)])]

drawBarLabel :: Config -> Bar a -> Diagram
drawBarLabel Config{..} Bar{..} = text labelSize label

instance Measurable a => Drawable (Bar a) where
  draw config@Config{..} Bar{..} =
    vcat (reverse (map (draw config) (zip barColors blocks)))

instance Drawable (SomeColor, Label) where
  draw Config{..} (color, string) =
    hsep labelSep [block color labelSize labelSize, text labelSize string]

block :: SomeColor -> Double -> Double -> Diagram
block color width height =
  lineColor white . fillColor color $ roundRectF width height 0.1

instance Measurable a => Drawable (SomeColor, Block a) where
  draw Config{..} (color, Value x) =
      hsep labelSep [block color barWidth (ratio * size x),
                     ctext labelSize (show x)]

  draw Config{..} (color,Interval{..}) =
    hsep labelSep [block color barWidth (ratio * size mean),
                   deviation,
                   ctext labelSize (show mean)]
   where deviation = translateY (ratio * size (mean-upper)) interval
         interval  = vcat [bound,
                           straight (pathFromVectors
                                      [(0,ratio * size (upper-lower))]),
                           bound]
         bound     = translateX (-labelSep/2)
                       (straight (pathFromVectors [(labelSep,0)]))

ctext :: Double -> String -> Diagram
ctext size string = translateY (-size/2) (text size string)
