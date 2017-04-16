
module PrintChangeset
where

import           CsvDiffImpl
import           Data.List   (groupBy, intercalate)
import           Data.Text   (Text)

changesetToString :: Changeset -> String
changesetToString = ppChangeset

ppChangeset :: Changeset -> String
ppChangeset changeset = "Adds:\n" ++ ppAdds (adds changeset) ++
  "Deletes:\n" ++ ppAdds (deletes changeset) ++
  "Modifications: \n" ++ ppModifications (modifications changeset) ++
    "Columns Added: " ++ show (columnsAdded changeset) ++ "\n" ++
    "Columns Deleted: " ++ show (columnsDeleted changeset)

ppAdds :: [IndexedRecord] -> String
ppAdds recs = "[\n" ++ Prelude.foldl (\acc rec -> acc ++ "\t" ++ show rec ++ "\n") "" recs ++ "]\n"

ppModifications :: [(RowIndex, ColumnIndex, Text, Text)] -> String
ppModifications mods = "[\n" ++ intercalate ",\n" (fmap ppModification (groupBy (\(idx1, _, _, _) (idx2, _, _, _) -> idx1==idx2) mods)) ++ "\n]\n"

-- print all modifications with a matching rowindex
ppModification :: [(RowIndex, ColumnIndex, Text, Text)] -> String
ppModification [] = ""
ppModification ((idx, colIdx, row1, row2):rst) = "\t(" ++ show idx ++ ", " ++ intercalate ", " (fmap ppModValues ((idx,colIdx,row1,row2):rst)) ++ ")"

ppModValues :: (RowIndex, ColumnIndex, Text, Text) -> String
ppModValues (_, colIdx, row1, row2) = show colIdx ++ ": " ++ show row1 ++ " -> " ++ show row2
