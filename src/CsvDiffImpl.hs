{-# LANGUAGE OverloadedStrings #-}

module CsvDiffImpl
where

import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Text       (Text, pack)
import           Safe
import           Text.CSV

data Changeset = Changeset
    {
     adds           :: [IndexedRecord],
     deletes        :: [IndexedRecord],
     -- unique identifier, column name, old value, new value
     modifications  :: [(RowIndex, ColumnIndex, Text, Text)],
     columnsAdded   :: [Text],
     columnsDeleted :: [Text]
    } deriving (Eq, Show)

data IndexedCsv = IndexedCsv
  {
    header  :: [Text],
    entries :: Map.Map Text [Text]
  } deriving (Eq, Show)

type RowIndex = Text
type ColumnIndex = Text
type Row = [Text]
type IndexedRecord = (RowIndex, Row)

-- Original CSV, New CSV, unique identifier column index
diff :: IndexedCsv -> IndexedCsv -> Changeset
diff = diffChangeset

changesetToString :: Changeset -> String
changesetToString = ppChangeset

ppChangeset :: Changeset -> String
ppChangeset changeset = "Adds:\n" ++ ppAdds (adds changeset) ++
  "Deletes:\n" ++ ppAdds (deletes changeset) ++
  "Modifications: \n" ++ ppModifications (modifications changeset) ++
  "Columns Added: " ++ show (columnsAdded changeset) ++ "\n" ++
  "Columns Deleted: " ++ show (columnsDeleted changeset)

ppAdds :: [IndexedRecord] -> String
ppAdds recs = "[\n" ++ foldl (\acc rec -> acc ++ "\t" ++ show rec ++ "\n") "" recs ++ "]\n"

ppModifications :: [(RowIndex, ColumnIndex, Text, Text)] -> String
ppModifications mods = "[\n" ++ intercalate ",\n" (fmap ppModification (groupBy (\(idx1, _, _, _) (idx2, _, _, _) -> idx1==idx2) mods)) ++ "\n]\n"

-- print all modifications with a matching rowindex
ppModification :: [(RowIndex, ColumnIndex, Text, Text)] -> String
ppModification [] = ""
ppModification ((idx, colIdx, row1, row2):rst) = "\t(" ++ show idx ++ ", " ++ intercalate ", " (fmap ppModValues ((idx,colIdx,row1,row2):rst)) ++ ")"

ppModValues :: (RowIndex, ColumnIndex, Text, Text) -> String
ppModValues (_, colIdx, row1, row2) = show colIdx ++ ": " ++ show row1 ++ " -> " ++ show row2

diffChangeset :: IndexedCsv -> IndexedCsv -> Changeset
diffChangeset original newfile =
  Changeset {
  adds = findAdds original newfile,
  deletes = findDeletes original newfile,
  modifications = findModifications original newfile,
  columnsAdded = findAddedColumns original newfile,
  columnsDeleted = findDeletedColumns original newfile
  }

findAdds :: IndexedCsv -> IndexedCsv -> [IndexedRecord]
findAdds original newfile = Map.toList (Map.difference (entries newfile) (entries original))

findDeletes :: IndexedCsv -> IndexedCsv -> [IndexedRecord]
findDeletes original newfile = findAdds newfile original

findAddedColumns :: IndexedCsv -> IndexedCsv -> [Text]
findAddedColumns original newfile =  header newfile \\ header original

findDeletedColumns :: IndexedCsv -> IndexedCsv -> [Text]
findDeletedColumns original newfile = findAddedColumns newfile original

findModifications :: IndexedCsv -> IndexedCsv -> [(RowIndex, ColumnIndex, Text, Text)]
findModifications original newfile = concatMap (\key -> compareFields key original newfile) (Map.keys (entries original))

-- for each entry that is in both maps - check each field - lookup header for given field, check header index in new csv, if exists check if fields match

compareFields :: RowIndex -> IndexedCsv -> IndexedCsv -> [(RowIndex, ColumnIndex, Text, Text)]
compareFields index original newfile = compareFieldsHelper index original newfile (header original) []

compareFieldsHelper :: RowIndex -> IndexedCsv -> IndexedCsv -> [ColumnIndex] -> [(RowIndex, ColumnIndex, Text, Text)] -> [(RowIndex, ColumnIndex, Text, Text)]
compareFieldsHelper _ _ _ [] results = results
compareFieldsHelper index original newfile (headerStr:rest) results =
  if differsMaybeStr (getField original index headerStr) (getField newfile index headerStr)
    then compareFieldsHelper index original newfile rest
      ((index, headerStr, fromMaybe "" (getField original index headerStr), fromMaybe "" (getField newfile index headerStr)) : results)
    else compareFieldsHelper index original newfile rest results

differsMaybeStr :: Maybe Text -> Maybe Text -> Bool
differsMaybeStr (Just s1) (Just s2) = s1 /= s2
differsMaybeStr _ _                 = False

getField :: IndexedCsv -> RowIndex -> Text -> Maybe Text
getField IndexedCsv{header = headerS, entries = entriesS} rowId columnName = getRowField (Map.lookup rowId entriesS) (getIndexForHeader headerS columnName)

getRowField :: Maybe [Text] -> Maybe Int -> Maybe Text
getRowField (Just str) (Just index) = atMay str index
getRowField _ _                     = Nothing

getIndexForHeader :: [Text] -> Text -> Maybe Int
getIndexForHeader headers value = elemIndex value headers

toIndexedCsv :: CSV -> String -> IndexedCsv
toIndexedCsv csvf indexColumn = IndexedCsv {header = fmap pack $ fromMaybe [] (headMay csvf),
  entries = csvToMap (getIndexForHeader (fmap pack $ fromMaybe [] $ headMay csvf) $ pack indexColumn) (tailMay csvf)}

csvToMap :: Maybe Int -> Maybe [Record] -> Map.Map Text [Text]
csvToMap (Just index) (Just liness) = Map.fromList (filterListMaybes (fmap (\line -> (getRowField (Just $ fmap pack line) (Just index) , fmap pack line)) liness))
csvToMap _ _ = Map.empty

filterListMaybes :: [(Maybe a, [a])] -> [(a, [a])]
filterListMaybes ((Just a, b) : rest) = (a,b) : filterListMaybes rest
filterListMaybes []                   = []
filterListMaybes (_ : rest)           = filterListMaybes rest
