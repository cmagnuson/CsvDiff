module Main where

import CsvDiff
import Text.ParserCombinators.Parsec
import Text.CSV
import System.Environment

main :: IO ()
main = do
  [file1', file2', indexColumn] <- getArgs
  file1 <- parseCSVFromFile file1'
  file2 <- parseCSVFromFile file2'
  putStrLn (csvdiff file1 file2 indexColumn)

csvdiff :: Either ParseError CSV -> Either ParseError CSV -> String -> String
csvdiff (Right csv1) (Right csv2) indexColumn = diff (toIndexedCsv csv1 indexColumn) (toIndexedCsv csv2 indexColumn)
csvdiff _ _ _ = "FIXME: error parsing"
