{-# LANGUAGE NamedFieldPuns, RecordWildCards, FlexibleInstances #-}

module Graphics.BarChart (

  -- ^ Bar chart datatypes

  Label, BarChart(..), Bar(..), Block(..),

  -- ^ Rendering bar charts

  Config(..), conf, render, diagram, Measurable(..),

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

render :: Measurable a => BarChart a -> IO ()
render = renderWith conf

renderWith :: Measurable a => Config -> BarChart a -> IO ()
renderWith config@Config{..} chart =
  renderAs PNG filename (Width (fromIntegral width)) . diagram config $ chart
 where (width,_) = chart_size

diagram :: Measurable a => Config -> BarChart a -> Diagram
diagram config@Config{..} chart@BarChart{..} =
  draw config{ ratio = hratio / wratio, font_size = font_size / wratio } chart
 where
  (width,height) = chart_size
  wratio = fromIntegral width / genericLength bars
  hratio = fromIntegral height / barChartHeight chart

barChartHeight :: Measurable a => BarChart a -> Double
barChartHeight BarChart{..} | null bars = 0
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
    pad font_size font_size $ vsepA font_size hcenter [header,body]
   where
    width   = genericLength bars
    height  = ratio * barChartHeight chart
    bar_sep = 1 - bar_ratio

    header  = vsepA font_size hcenter [title,legend]
    title   = text (1.5*font_size) caption
    legend  = hcat (map (draw config) (zip bar_colors blockLabels))

    body    = vsepA font_size hcenter
                [vcat [yBars, xaxis, xlabels], text font_size xlabel]

    yBars   = hdistribA (bar_sep/2) left bottom [yaxis, cols]
    yaxis   = vsep (font_size/2)
                [text font_size ylabel, straight (pathFromVectors [(0,height)])]
    cols    = sideBySide (map (draw config) bars)

    xaxis   = straight (pathFromVectors [(width,0)]) <> hspace 0.5
    xlabels = vcat [vspace (font_size/2),
                    hcat [hspace (bar_sep/2),
                          sideBySide (map (drawBarLabel config) bars)]]

instance Drawable (SomeColor, Label) where
  draw Config{..} (color, string) =
    hcat [block color font_size font_size, text font_size string]

block :: SomeColor -> Double -> Double -> Diagram
block color width height =
  lineColor white . fillColor color $ roundRectF width height 0.1

instance Measurable a => Drawable (Bar a) where
  draw config@Config{..} Bar{..} =
    vcat (reverse (map (draw config) (zip bar_colors blocks)))

instance Measurable a => Drawable (SomeColor, Block a) where
  draw Config{..} (color, Value x) = hcat [bar, label]
   where
    bar   = block color bar_ratio (ratio * size x)
    label = translateX (font_size/2) (ctext font_size (show x))

  draw Config{..} (color,Interval{..}) = hcat [bar, deviation, label]
   where
    bar       = block color bar_ratio (ratio * size mean)
    deviation = translateY (ratio * size (mean-upper)) interval
    interval  = vcat [bound,line,bound]
    bound     = translateX (-font_size/2) $
                  straight (pathFromVectors [(font_size,0)])
    line      = straight (pathFromVectors [(0,ratio * size (upper-lower))])
    label     = translateX (-font_size/2) (ctext font_size (show mean))

ctext :: Double -> String -> Diagram
ctext size string = translateY (-size/2) (text size string)

drawBarLabel :: Config -> Bar a -> Diagram
drawBarLabel Config{..} Bar{..} = text font_size label

sideBySide [] = nil
sideBySide ds = hdistribA 1 left bottom (init ds) <> last ds