{-# LANGUAGE OverloadedStrings #-}

module Main where

import           CsvDiff
import           Lucid
import           System.Environment
import           Text.CSV
import           Text.ParserCombinators.Parsec

main :: IO ()
main = do
  [file1', file2', indexColumn] <- getArgs
  file1 <- parseCSVFromFile file1'
  file2 <- parseCSVFromFile file2'
  renderToFile "test.html" $ changesetToHtml (csvdiff file1 file2 indexColumn)
  putStrLn $ changesetToString (csvdiff file1 file2 indexColumn)
