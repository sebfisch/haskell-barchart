{-# LANGUAGE NamedFieldPuns, RecordWildCards, DeriveDataTypeable #-}

import Graphics.BarChart hiding ( Intervals, BarChart )
import Graphics.BarChart.Types ( readColor )

import Data.Char ( toLower )
import Data.Maybe ( fromMaybe )

import Control.Monad ( when, forM_ )

import System.FilePath

import System.Console.CmdArgs

import qualified Graphics.Rendering.Diagrams as D

data BarChart
  = Blocks      { out_file, title, xlabel, ylabel :: String,
                  file_type :: FileType,
                  division, colors :: String,
                  width, height, label_size :: Int,
                  bar_width :: Double,
                  in_files :: [String] }
  | Intervals   { out_file, title, xlabel, ylabel :: String,
                  file_type :: FileType,
                  division, colors :: String,
                  width, height, label_size :: Int,
                  bar_width :: Double,
                  in_files :: [String] }
  | Criterion   { breakdown :: Breakdown,

                  out_file, title, xlabel, ylabel :: String,
                  file_type :: FileType,
                  division, colors :: String,
                  width, height, label_size :: Int,
                  bar_width :: Double,
                  in_files :: [String] }
  | Progression { breakdown :: Breakdown,

                  out_file, title, xlabel, ylabel :: String,
                  file_type :: FileType,
                  division, colors :: String,
                  width, height, label_size :: Int,
                  bar_width :: Double,
                  in_files :: [String] }
 deriving (Show,Data,Typeable)

data FileType = Guess_File_Type | PNG | SVG | PDF | PS
 deriving (Eq,Show,Data,Typeable)

suffixOf :: FileType -> String
suffixOf PNG = "png"
suffixOf SVG = "svg"
suffixOf PDF = "pdf"
suffixOf PS  = "ps"
suffixOf _   = ""

fromFileType :: FileType -> D.OutputType
fromFileType PNG = D.PNG
fromFileType SVG = D.SVG
fromFileType PDF = D.PDF
fromFileType PS  = D.PS
fromFileType _   = error "fromFileType: cannot convert guessed file type"

replaceUnknownFileType :: FileType -> FileType -> FileType
replaceUnknownFileType Guess_File_Type t = t
replaceUnknownFileType t               _ = t

data Breakdown = Summary | Summary_Comparison | Benchmark_Comparison
 deriving (Eq,Show,Data,Typeable)

blocksMode :: BarChart
blocksMode =
  Blocks {
    out_file   = outFile conf
                    &= help "Name of generated file"
                    &= typFile,
    file_type  = enum [Guess_File_Type
                           &= help "Guess output file type by name (default)",
                       PNG &= help "Generate .png file",
                       SVG &= help "Generate .svg file"
                           &= explicit
                           &= name "svg",
                       PDF &= help "Generate .pdf file",
                       PS  &= help "Generate .ps file"],
    title      = caption conf
                    &= help "Title of bar chart"
                    &= typString,
    xlabel     = xLabel conf
                    &= help "Label of x axis"
                    &= typString,
    ylabel     = xLabel conf
                    &= help "Label of y axis"
                    &= typString,
    division   = "" &= help "Labels separated by whitespace"
                    &= typStrings,
    colors     = "" &= help "Color names separated by whitespace"
                    &= typStrings,
    width      = width
                    &= help "Width of generated bar chart"
                    &= typ "NUM",
    height     = height
                    &= help "Height of generated bar chart"
                    &= typ "NUM",
    label_size = 12 &= help "Font size used for labels"
                    &= typ "NUM",
    bar_width  = barRatio conf
                    &= help "Bar width between 0 and 1"
                    &= name "W"
                    &= typ "FLOAT",
    in_files   = [] &= typ "FILES"
                    &= args }
 where (width,height) = dimensions conf

intervalsMode :: BarChart
intervalsMode = Intervals {}

criterionMode :: BarChart
criterionMode =
  Criterion {
    breakdown  = enum [Summary
                        &= help "Show benchmark summary (default)",
                       Summary_Comparison
                        &= help "Compare different benchmark summaries"
                        &= name "s",
                       Benchmark_Comparison
                        &= help "Compare different benchmarks"
                        &= name "b"] }

progressionMode :: BarChart
progressionMode =
  Progression {
    breakdown  = enum [Summary_Comparison
                        &= help "Breakdown chart by benchmark summary (default)"
                        &= name "s",
                       Benchmark_Comparison
                        &= help "Breakdown chart by benchmarks"
                        &= name "b"] }

typString, typStrings :: Ann
typString  = typ "STRING"
typStrings = typ "STRINGS"

execModes :: [BarChart]
execModes = [blocksMode &= auto,
             intervalsMode, criterionMode, progressionMode]

exitIf :: String -> Bool -> IO ()
exitIf msg cond = when cond (error msg)

main :: IO ()
main = do execMode <- cmdArgs (modes execModes)
          exitIf "no input files given" $ null (in_files execMode)
          dispatch execMode

dispatch :: BarChart -> IO ()
dispatch mode@Blocks{..} =
  forM_ in_files $ \in_file ->
    writeMultiBarChart
      (config (guessDefaults in_file mode))
      in_file
      (words division)

dispatch mode@Intervals{..} =
  forM_ in_files $ \in_file ->
    writeMultiBarIntervalChart
      (config (guessDefaults in_file mode))
      in_file
      (words division)

dispatch mode@Criterion{..} =
  case breakdown of
    Summary ->
      forM_ in_files $ \in_file ->
        writeCriterionChart
          (config (guessBenchmarkDefaults in_file mode))
          in_file
    Summary_Comparison ->
      writeComparisonChart False
        (config (guessBenchmarkDefaults "summaries.csv" mode))
        in_files
    Benchmark_Comparison ->
      writeComparisonChart True
        (config (guessBenchmarkDefaults "benchmarks.csv" mode))
        in_files

dispatch mode@Progression{..} =
  case breakdown of
    Summary_Comparison ->
      forM_ in_files $ \in_file ->
        writeProgressionChart True
          (config (guessBenchmarkDefaults in_file mode))
          in_file
          (words division)
    Benchmark_Comparison ->
      forM_ in_files $ \in_file ->
        writeProgressionChart False
          (config (guessBenchmarkDefaults in_file mode))
          in_file
          (words division)

guessDefaults :: FilePath -> BarChart -> BarChart
guessDefaults in_file = guessColors . guessTitle . guessFileType . guessOutFile
 where
  guessOutFile mode =
    mode { out_file = out_file mode ? replaceExtension in_file suffix }
   where suffix = suffixOf (file_type mode) ? ".png"

  guessFileType mode =
    mode { file_type = replaceUnknownFileType (file_type mode) $
                         fromMaybe (error $ "unsupported type: " ++ suffix) $
                           lookup suffix fileTypes }
   where suffix    = map toLower $ takeExtension (out_file mode)
         fileTypes = [(".png",PNG),(".svg",SVG),(".pdf",PDF),(".ps",PS)]

  guessTitle mode =
    mode { title = title mode ? takeBaseName (out_file mode) }

  guessColors mode =
    mode { colors = colors mode ? "forestgreen firebrick midnightblue" }

guessBenchmarkDefaults :: FilePath -> BarChart -> BarChart
guessBenchmarkDefaults in_file = guessAxis . guessDefaults in_file
 where
  guessAxis mode = mode { xlabel = xlabel mode ? "benchmark",
                          ylabel = ylabel mode ? "run time" }

config :: BarChart -> Config
config mode = Config {
  outFile = out_file mode,
  outputType = fromFileType $ file_type mode,
  caption = title mode, xLabel = xlabel mode, yLabel = ylabel mode,
  barColors = map readColor . words $ colors mode,
  dimensions = (width mode,height mode),
  ratio = 1.0,
  fontSize = fromIntegral $ label_size mode,
  barRatio = bar_width mode }

(?) :: String -> String -> String
"" ? s = s
s  ? _ = s
