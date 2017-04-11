module CsvDiff (diff, toIndexedCsv, Changeset)
where

import qualified Data.Map.Strict as Map
import Safe
import Data.List
import Data.Maybe
import Text.CSV

data Changeset = Changeset
    {
     adds :: [IndexedRecord],
     deletes :: [IndexedRecord],
     modifications :: [(RowIndex, ColumnIndex, Row, Row)],
     columnsAdded :: [String],
     columnsDeleted :: [String]
    } deriving (Eq, Show)

data IndexedCsv = IndexedCsv
  {
    header :: [String],
    entries :: Map.Map String [String]
  } deriving (Eq, Show)

type RowIndex = String
type ColumnIndex = String
type Row = [String]
type IndexedRecord = (RowIndex, Row)

-- Original CSV, New CSV, unique identifier column index
diff :: IndexedCsv -> IndexedCsv -> Changeset
diff csv1 csv2 = diffChangeset csv1 csv2

changesetToString :: Changeset -> String
changesetToString = show

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

findAddedColumns :: IndexedCsv -> IndexedCsv -> [String]
findAddedColumns original newfile =  header newfile \\ header original

findDeletedColumns :: IndexedCsv -> IndexedCsv -> [String]
findDeletedColumns original newfile = findAddedColumns newfile original

findModifications :: IndexedCsv -> IndexedCsv -> [(RowIndex, ColumnIndex, Row, Row)]
findModifications original newfile = concatMap (\key -> compareFields key original newfile) (Map.keys (entries original))

-- for each entry that is in both maps - check each field - lookup header for given field, check header index in new csv, if exists check if fields match

compareFields :: RowIndex -> IndexedCsv -> IndexedCsv -> [(RowIndex, ColumnIndex, Row, Row)]
compareFields index original newfile = compareFieldsHelper index original newfile (header original) []

compareFieldsHelper :: RowIndex -> IndexedCsv -> IndexedCsv -> [ColumnIndex] -> [(RowIndex, ColumnIndex, Row, Row)] -> [(RowIndex, ColumnIndex, Row, Row)]
compareFieldsHelper index original newfile [] results = results
compareFieldsHelper index original newfile (headerStr:rest) results =
  if differsMaybeStr (getField original index headerStr) (getField newfile index headerStr)
    then compareFieldsHelper index original newfile rest
      ((index, headerStr, fromMaybe [] (Map.lookup index (entries original)) :: Row, fromMaybe [] (Map.lookup index (entries newfile)) :: Row) : results)
    else compareFieldsHelper index original newfile rest results

differsMaybeStr :: Maybe String -> Maybe String -> Bool
differsMaybeStr (Just s1) (Just s2) = s1 /= s2
differsMaybeStr _ _ = False

getField :: IndexedCsv -> RowIndex -> String -> Maybe String
getField IndexedCsv{header = header, entries = entries} rowId columnName = getRowField (Map.lookup rowId entries) (getIndexForHeader header columnName)

getRowField :: Maybe [String] -> Maybe Int -> Maybe String
getRowField (Just str) (Just index) = atMay str index
getRowField _ _ = Nothing

getIndexForHeader :: [String] -> String -> Maybe Int
getIndexForHeader header value = elemIndex value header

toIndexedCsv :: CSV -> String -> IndexedCsv
toIndexedCsv csv indexColumn = IndexedCsv {header = fromMaybe [] (headMay csv),
  entries = csvToMap (getIndexForHeader (fromMaybe [] (headMay csv)) indexColumn) (tailMay csv)}

csvToMap :: Maybe Int -> Maybe [[String]] -> Map.Map String [String]
csvToMap (Just index) (Just lines) = Map.fromList (filterListMaybes (fmap (\line -> (getRowField (Just line) (Just index) , line)) lines))
csvToMap _ _ = Map.empty

filterListMaybes :: [(Maybe a, [a])] -> [(a, [a])]
filterListMaybes ((Just a, b) : rest) = (a,b) : filterListMaybes rest
filterListMaybes [] = []
filterListMaybes (_ : rest) = filterListMaybes rest
