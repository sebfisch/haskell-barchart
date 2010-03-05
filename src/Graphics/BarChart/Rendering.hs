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
  do renderAs outputType outFile (Width (fromIntegral width))
       . diagram config
       $ chart
     putStrLn $ "Chart written in file " ++ outFile
 where (width,_) = dimensions

diagram :: Measurable a => Config -> BarChart a -> Diagram
diagram config@Config{..} chart@BarChart{..} =
  drawBarChart config{ ratio     = hratio / wratio,
                       fontSize = fontSize / wratio }
               chart
 where
  (width,height) = dimensions
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
  pad fontSize fontSize $ vsepA fontSize hcenter [header,body]
 where
  width   = genericLength bars
  height  = ratio * barChartHeight chart
  bar_sep = 1 - barRatio

  header  = vsepA fontSize hcenter [title,legend]
  title   = text (1.5*fontSize) caption
  legend  = hcat (zipWith (drawDescr config) (roll barColors) block_labels)

  body    = vcat [yBars, xaxis, xLabels]

  yBars   = hdistribA (bar_sep/2) left bottom [yaxis, cols]
  yaxis   = vsep (fontSize/2)
              [text fontSize yLabel,
               straight (pathFromVectors [(0,height)])]
  cols    = sideBySide left (map (drawBar config) bars)

  xaxis   = hsep (fontSize/2)
              [straight (pathFromVectors [(width,0)]), text fontSize xLabel]
  xLabels = vspace (fontSize/2)
         // sideBySide vcenter (map (drawBarLabel config) bars)

roll :: [SomeColor] -> [SomeColor]
roll colors | null colors = cycle [readColor "black"]
            | otherwise   = cycle colors
                                
drawDescr :: Config -> SomeColor -> Label -> Diagram
drawDescr Config{..} color string =
  block color fontSize fontSize <> text fontSize string

block :: SomeColor -> Double -> Double -> Diagram
block color width height =
  lineColor white . fillColor color $ roundRectF width height 0.1

drawBar :: Measurable a => Config -> Bar a -> Diagram
drawBar config@Config{..} Bar{..} =
  vcat . reverse $ zipWith (drawBlock config) (roll barColors) blocks

drawBlock :: Measurable a => Config -> SomeColor -> Block a -> Diagram

drawBlock Config{..} color (Value x) = hcatA bottom [bar, label]
 where
  bar   = block color barRatio (ratio * size x)
  label = translateX (fontSize/2)
        . translateY (fontSize - ratio * size x)
        . ctext fontSize
        $ show x

drawBlock Config{..} color Interval{..} = hcatA bottom [bar, deviation, label]
 where
  bar       = block color barRatio (ratio * size mean)
  deviation = translateY (-ratio * size lower) interval
  interval  = vcat [bound,line,bound]
  bound     = translateX (-fontSize/2) . straight $
                pathFromVectors [(fontSize,0)]
  line      = straight $ pathFromVectors [(0,ratio * size (upper-lower))]
  label     = translateX (-fontSize/2)
            . translateY (fontSize - ratio * size mean)
            . ctext fontSize
            $ show mean

ctext :: Double -> String -> Diagram
ctext size string = translateY (-size/2) $ text size string

drawBarLabel :: Config -> Bar a -> Diagram
drawBarLabel Config{..} Bar{..} = text fontSize label

sideBySide valign ds = hdistribA 1 valign bottom ds
