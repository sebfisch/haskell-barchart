{-# LANGUAGE NamedFieldPuns, RecordWildCards, DeriveDataTypeable #-}

import Graphics.BarChart hiding ( Intervals )
import Graphics.BarChart.Types ( readColor )

import Data.Char ( toLower )
import Data.Maybe ( fromMaybe )

import Control.Monad ( when, forM_ )

import System.FilePath

import System.Console.CmdArgs

import qualified Graphics.Rendering.Diagrams as D

data ExecMode
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

blocksMode =
  Blocks {
    out_file   = outFile conf
                    &= text "Name of generated file"
                    &  typFile,
    file_type  = Guess_File_Type `enum`
                    [Guess_File_Type
                         &= text "Guess output file type by name (default)",
                     PNG &= text "Generate .png file",
                     SVG &= text "Generate .svg file",
                     PDF &= text "Generate .pdf file",
                     PS  &= text "Generate .ps file"],
    title      = caption conf
                    &= text "Title of bar chart"
                    &  typString,
    xlabel     = xLabel conf
                    &= text "Label of x axis"
                    &  typString,
    ylabel     = xLabel conf
                    &= text "Label of y axis"
                    &  typString,
    division  = "" &= text "Labels separated by whitespace"
                    &  typStrings,
    colors     = "" &= text "Color names separated by whitespace"
                    &  typStrings,
    width      = width
                    &= text "Width of generated bar chart"
                    &  typ "NUM",
    height     = height
                    &= text "Height of generated bar chart"
                    &  typ "NUM",
    label_size = 12 &= text "Font size used for labels"
                    &  typ "NUM",
    bar_width  = barRatio conf
                    &= text "Bar width between 0 and 1"
                    &  flag "W"
                    &  typ "FLOAT",
    in_files   = [] &= text "CVS files with data to visualise"
                    &  args }
 where (width,height) = dimensions conf

-- cannot reuse flag attributes in cmdargs :(
intervalsMode =
  Intervals {
    out_file   = outFile conf
                    &= text "Name of generated file"
                    &  typFile,
    file_type  = Guess_File_Type `enum`
                    [Guess_File_Type
                         &= text "Guess output file type by name (default)",
                     PNG &= text "Generate .png file",
                     SVG &= text "Generate .svg file",
                     PDF &= text "Generate .pdf file",
                     PS  &= text "Generate .ps file"],
    title      = caption conf
                    &= text "Title of bar chart"
                    &  typString,
    xlabel     = xLabel conf
                    &= text "Label of x axis"
                    &  typString,
    ylabel     = xLabel conf
                    &= text "Label of y axis"
                    &  typString,
    division  = "" &= text "Labels separated by whitespace"
                    &  typStrings,
    colors     = "" &= text "Color names separated by whitespace"
                    &  typStrings,
    width      = width
                    &= text "Width of generated bar chart"
                    &  typ "NUM",
    height     = height
                    &= text "Height of generated bar chart"
                    &  typ "NUM",
    label_size = 12 &= text "Font size used for labels"
                    &  typ "NUM",
    bar_width  = barRatio conf
                    &= text "Bar width between 0 and 1"
                    &  flag "W"
                    &  typ "FLOAT",
    in_files   = [] &= text "CVS files with data to visualise"
                    &  args }
 where (width,height) = dimensions conf

criterionMode =
  Criterion {
    out_file   = outFile conf
                    &= text "Name of generated file"
                    &  typFile,
    file_type  = Guess_File_Type `enum`
                    [Guess_File_Type
                         &= text "Guess output file type by name (default)",
                     PNG &= text "Generate .png file",
                     SVG &= text "Generate .svg file",
                     PDF &= text "Generate .pdf file",
                     PS  &= text "Generate .ps file"],
    title      = caption conf
                    &= text "Title of bar chart"
                    &  typString,
    xlabel     = xLabel conf
                    &= text "Label of x axis"
                    &  typString,
    ylabel     = xLabel conf
                    &= text "Label of y axis"
                    &  typString,
    division  = "" &= text "Labels separated by whitespace"
                    &  typStrings,
    colors     = "" &= text "Color names separated by whitespace"
                    &  typStrings,
    width      = width
                    &= text "Width of generated bar chart"
                    &  typ "NUM",
    height     = height
                    &= text "Height of generated bar chart"
                    &  typ "NUM",
    label_size = 12 &= text "Font size used for labels"
                    &  typ "NUM",
    bar_width  = barRatio conf
                    &= text "Bar width between 0 and 1"
                    &  flag "W"
                    &  typ "FLOAT",
    in_files   = [] &= text "CVS files with data to visualise"
                    &  args,
    breakdown  = Summary `enum`
                  [Summary
                    &= text "Show benchmark summary (default)",
                   Summary_Comparison
                    &= text "Compare different benchmark summaries"
                    &  flag "s",
                   Benchmark_Comparison
                    &= text "Compare different benchmarks"
                    &  flag "b"] }
 where (width,height) = dimensions conf

progressionMode =
  Progression {
    out_file   = outFile conf
                    &= text "Name of generated file"
                    &  typFile,
    file_type  = Guess_File_Type `enum`
                    [Guess_File_Type
                         &= text "Guess output file type by name (default)",
                     PNG &= text "Generate .png file",
                     SVG &= text "Generate .svg file",
                     PDF &= text "Generate .pdf file",
                     PS  &= text "Generate .ps file"],
    title      = caption conf
                    &= text "Title of bar chart"
                    &  typString,
    xlabel     = xLabel conf
                    &= text "Label of x axis"
                    &  typString,
    ylabel     = xLabel conf
                    &= text "Label of y axis"
                    &  typString,
    division  = "" &= text "Labels separated by whitespace"
                    &  typStrings,
    colors     = "" &= text "Color names separated by whitespace"
                    &  typStrings,
    width      = width
                    &= text "Width of generated bar chart"
                    &  typ "NUM",
    height     = height
                    &= text "Height of generated bar chart"
                    &  typ "NUM",
    label_size = 12 &= text "Font size used for labels"
                    &  typ "NUM",
    bar_width  = barRatio conf
                    &= text "Bar width between 0 and 1"
                    &  flag "W"
                    &  typ "FLOAT",
    in_files   = [] &= text "CVS files with data to visualise"
                    &  args,
    breakdown  = Summary_Comparison `enum`
                  [Summary_Comparison
                    &= text "Breakdown chart by benchmark summary (default)"
                    & flag "s",
                   Benchmark_Comparison
                    &= text "Breakdown chart by benchmarks"
                    &  flag "b"] }
 where (width,height) = dimensions conf

typString  = typ "STRING"
typStrings = typ "STRINGS"

execModes = [blocksMode &= defMode,
             intervalsMode, criterionMode, progressionMode]

exitIf msg cond = when cond (error msg)

main = do execMode <- cmdArgs "Bar Chart 0.0" (map mode execModes)
          exitIf "no input files given" $ null (in_files execMode)
          dispatch execMode

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

guessDefaults :: FilePath -> ExecMode -> ExecMode
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

guessBenchmarkDefaults in_file = guessAxis . guessDefaults in_file
 where
  guessAxis mode = mode { xlabel = xlabel mode ? "benchmark",
                          ylabel = ylabel mode ? "run time" }

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
