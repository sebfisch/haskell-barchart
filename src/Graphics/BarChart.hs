module Graphics.BarChart (

  -- ^ Bar chart datatypes

  Label, BarChart(..), Bar(..), Block(..),

  -- ^ Rendering bar charts

  Config(..), conf, render, diagram, Measurable(..),

  ) where

import Graphics.BarChart.Types
import Graphics.BarChart.Rendering
import Graphics.BarChart.Parser
import Graphics.BarChart.Parser.Criterion
