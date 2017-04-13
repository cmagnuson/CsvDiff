module CsvDiff (diff, toIndexedCsv, Changeset, changesetToString)
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
     -- unique identifier, column name, old value, new value
     modifications :: [(RowIndex, ColumnIndex, String, String)],
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

ppModifications :: [(RowIndex, ColumnIndex, String, String)] -> String
ppModifications mods = "[\n" ++ intercalate ",\n" (fmap ppModification (groupBy (\(idx1, _, _, _) (idx2, _, _, _) -> idx1==idx2) mods)) ++ "\n]\n"

-- print all modifications with a matching rowindex
ppModification :: [(RowIndex, ColumnIndex, String, String)] -> String
ppModification [] = ""
ppModification ((idx, colIdx, row1, row2):rst) = "\t(" ++ idx ++ ", " ++ intercalate ", " (fmap ppModValues ((idx,colIdx,row1,row2):rst)) ++ ")"

ppModValues :: (RowIndex, ColumnIndex, String, String) -> String
ppModValues (_, colIdx, row1, row2) = colIdx ++ ": " ++ show row1 ++ " -> " ++ show row2

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

findModifications :: IndexedCsv -> IndexedCsv -> [(RowIndex, ColumnIndex, String, String)]
findModifications original newfile = concatMap (\key -> compareFields key original newfile) (Map.keys (entries original))

-- for each entry that is in both maps - check each field - lookup header for given field, check header index in new csv, if exists check if fields match

compareFields :: RowIndex -> IndexedCsv -> IndexedCsv -> [(RowIndex, ColumnIndex, String, String)]
compareFields index original newfile = compareFieldsHelper index original newfile (header original) []

compareFieldsHelper :: RowIndex -> IndexedCsv -> IndexedCsv -> [ColumnIndex] -> [(RowIndex, ColumnIndex, String, String)] -> [(RowIndex, ColumnIndex, String, String)]
compareFieldsHelper index original newfile [] results = results
compareFieldsHelper index original newfile (headerStr:rest) results =
  if differsMaybeStr (getField original index headerStr) (getField newfile index headerStr)
    then compareFieldsHelper index original newfile rest
      ((index, headerStr, fromMaybe "" (getField original index headerStr), fromMaybe "" (getField newfile index headerStr)) : results)
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
