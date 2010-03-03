{-# LANGUAGE TypeFamilies, NamedFieldPuns, RecordWildCards #-}

module Graphics.BarChart.Parser where

import Text.CSV

import Data.List ( nub )
import Data.Maybe ( fromJust )

import Graphics.BarChart.Types

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

instance Drawable (MultiBarIntervals a)
 where
  type Value (MultiBarIntervals a) = a

  chart caption xlabel ylabel (MBIntervals block_labels pairs) = BarChart{..}
   where bars = map (uncurry mkBar) pairs

         mkBar label ints = Bar{..}
          where blocks = map mkInterval ints
                mkInterval (mean,lower,upper) = Interval{..}

parseMultiBars :: Read a => CSV -> MultiBars a
parseMultiBars = MultiBars . map parseRecord
 where parseRecord (label:values) = (read label,map read values)

parseIntervals :: Read a => CSV -> Intervals a
parseIntervals = Intervals . map parseRecord
 where parseRecord [label,m,l,u] = (read label,(read m, read l, read u))

mergeIntervals :: Num a => [(Label,Intervals a)] -> MultiBarIntervals a
mergeIntervals xs =
  MBIntervals block_labels [ (label,intervals label) | label <- bar_labels ]
 where
  bar_labels   = map fst xs
  block_labels = nub (concatMap ((\ (Intervals ys) -> map fst ys) . snd) xs)

  intervals l  = map (maybe (0,0,0) id . flip lookup ys) block_labels
   where Intervals ys = fromJust (lookup l xs)
