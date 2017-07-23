{-# LANGUAGE OverloadedStrings #-}

module MergeImpl
where

import           CsvDiffImpl
import           Data.Text

data Conflict = MultipleFieldModifications ((RowIndex, ColumnIndex, Text, Text), (RowIndex, ColumnIndex, Text, Text))
              | ModifyFieldAndDeleteColumn ((RowIndex, ColumnIndex, Text, Text), Text)
              | ModifyAndDeleteRow ((RowIndex, ColumnIndex, Text, Text), IndexedRecord)

-- Three way merge producing a Changeset from Original CSV to final CSV list of merge conflicts
merge :: IndexedCsv -> IndexedCsv -> IndexedCsv -> (Changeset, [Conflict])
merge original left right = mergeChangeset (diff original left) (diff original right)

mergeChangeset :: Changeset -> Changeset -> (Changeset, [Conflict])
mergeChangeset = undefined
