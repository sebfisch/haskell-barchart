{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Graphics.BarChart.Rendering where

import Data.List ( genericLength )

import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Types ( SomeColor )

import Graphics.BarChart.Types

render :: Measurable a => BarChart a -> IO ()
render = renderWith conf

renderWith :: Measurable a => Config -> BarChart a -> IO ()
renderWith config@Config{..} chart =
  renderAs PNG filename (Width (fromIntegral width)) . diagram config $ chart
 where (width,_) = chart_size

diagram :: Measurable a => Config -> BarChart a -> Diagram
diagram config@Config{..} chart@BarChart{..} =
  drawBarChart config{ ratio     = hratio / wratio,
                       font_size = font_size / wratio }
               chart
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

drawBarChart :: Measurable a => Config -> BarChart a -> Diagram
drawBarChart config@Config{..} chart@BarChart{..} =
  pad font_size font_size $ vsepA font_size hcenter [header,body]
 where
  width   = genericLength bars
  height  = ratio * barChartHeight chart
  bar_sep = 1 - bar_ratio

  header  = vsepA font_size hcenter [title,legend]
  title   = text (1.5*font_size) caption
  legend  = hcat (zipWith (drawDescr config) bar_colors block_labels)

  body    = vsepA font_size hcenter
              [vcat [yBars, xaxis, xlabels], text font_size xlabel]

  yBars   = hdistribA (bar_sep/2) left bottom [yaxis, cols]
  yaxis   = vsep (font_size/2)
              [text font_size ylabel, straight (pathFromVectors [(0,height)])]
  cols    = sideBySide (map (drawBar config) bars)

  xaxis   = straight (pathFromVectors [(width,0)]) <> hspace 0.5
  xlabels = vspace (font_size/2)
         // hspace (bar_sep/2) <> sideBySide (map (drawBarLabel config) bars)

drawDescr :: Config -> SomeColor -> Label -> Diagram
drawDescr Config{..} color string = block color font_size font_size
                                 // text font_size string

block :: SomeColor -> Double -> Double -> Diagram
block color width height =
  lineColor white . fillColor color $ roundRectF width height 0.1

drawBar :: Measurable a => Config -> Bar a -> Diagram
drawBar config@Config{..} Bar{..} =
  vcat . reverse $ zipWith (drawBlock config) bar_colors blocks

drawBlock :: Measurable a => Config -> SomeColor -> Block a -> Diagram

drawBlock Config{..} color (Value x) = hcat [bar, label]
 where
  bar   = block color bar_ratio (ratio * size x)
  label = translateX (font_size/2) . ctext font_size $ show x

drawBlock Config{..} color Interval{..} = hcat [bar, deviation, label]
 where
  bar       = block color bar_ratio (ratio * size mean)
  deviation = translateY (ratio * size (mean-upper)) interval
  interval  = vcat [bound,line,bound]
  bound     = translateX (-font_size/2) . straight $
                pathFromVectors [(font_size,0)]
  line      = straight $ pathFromVectors [(0,ratio * size (upper-lower))]
  label     = translateX (-font_size/2) . ctext font_size $ show mean

ctext :: Double -> String -> Diagram
ctext size string = translateY (-size/2) $ text size string

drawBarLabel :: Config -> Bar a -> Diagram
drawBarLabel Config{..} Bar{..} = text font_size label

sideBySide [] = nil
sideBySide ds = hdistribA 1 left bottom (init ds) <> last ds
