{-# LANGUAGE OverloadedStrings #-}
module Webapp where

import           Web.Spock
import           Web.Spock.Config

import           Control.Monad.Trans
import           CsvDiff
import           Data.Either
import qualified Data.HashMap.Strict as HM
import           Data.IORef
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text           as T
import           Text.CSV
import           Web.Spock.Lucid

data MySession = EmptySession
data MyAppState = DummyAppState ()

main :: IO ()
main =
    do spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ())
       runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app =
    do get root $
           text "Hello World!"
       post "csvdiff" $
            do fileHM       <- files
               index        <- param' "index"
               let file1' = HM.lookup "file1" fileHM
                   file2' = HM.lookup "file2" fileHM
                in do
                   file1  <- liftIO $ parseCSVFromFile $ uf_tempLocation (fromJust file1')
                   file2  <- liftIO $ parseCSVFromFile $ uf_tempLocation (fromJust file2')
                  --  case file1 of
                  --    Left parseError -> text $ T.pack $ show parseError
                  --    Right csv       -> text $ T.pack $ show csv
                  --  text (T.pack $ show file1)
                   lucid( changesetToHtml (csvdiff file1 file2 index) )
