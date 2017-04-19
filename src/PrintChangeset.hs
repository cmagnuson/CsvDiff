{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module PrintChangeset
where

import           CsvDiffImpl
import           Data.List   (foldl', groupBy)
import           Data.Monoid
import           Data.Text   (Text, intercalate, pack, unpack)
import           Lucid

changesetToString :: Changeset -> String
changesetToString = ppChangeset

changesetToHtml :: Changeset -> Html ()
changesetToHtml = ppChangesetHtml

ppChangesetHtml :: Changeset -> Html ()
ppChangesetHtml changeset =
  html_
  (do
    head_
      (do title_ "Changeset"
          style_ [type_ "text/css"] cssStyle
      )

    body_
      (do

      h2_ "CsvDiff"
      ul_ (do
        li_  (a_ [href_ "#addedColumns"] "Columns Added")
        li_  (a_ [href_ "#deletedColumns"] "Columns Deleted")
        li_  (a_ [href_ "#adds"] "Adds")
        li_  (a_ [href_ "#deletes"] "Deletes")
        li_  (a_ [href_ "#modifications"] "Modifications")
        )

      a_ [name_ "addedColumns"] ""
      h3_ (p_ "Columns Added: ")
      ul_ $ mapM_ (li_ . toHtml) (columnsAdded changeset)

      a_ [name_ "deletedColumns"] ""
      h3_ (p_ "Columns Deleted: ")
      ul_ $ mapM_ (li_ . toHtml) (columnsAdded changeset)

      a_ [name_ "adds"] ""
      h3_ (p_ "Adds: ")
      ul_ $ makeTable (adds changeset) (newFile changeset)

      a_ [name_ "deletes"] ""
      h3_ (p_ "Deletes: ")
      ul_ $ makeTable (deletes changeset) (oldFile changeset)

      a_ [name_ "modifications"] ""
      h3_ (p_ "Modifications: ")
      ppHtmlModifications (modifications changeset)
      ))

ppHtmlModifications :: [(RowIndex, ColumnIndex, Text, Text)] -> Html ()
ppHtmlModifications mods = mapM_ ppHtmlModification (groupBy (\(idx1, _, _, _) (idx2, _, _, _) -> idx1==idx2) mods)

ppHtmlModification :: [(RowIndex, ColumnIndex, Text, Text)] -> Html ()
ppHtmlModification mo = do h4_ (toHtml $ printModificationId mo);
                            table_ [class_ "gridtable"]
                            (mapM_ ppHtmlModificationTableRow mo)

printModificationId :: [(RowIndex, ColumnIndex, Text, Text)] -> Text
printModificationId ((idx, _, _, _):_) = idx
printModificationId _                  = ""

ppHtmlModificationTableRow :: (RowIndex, ColumnIndex, Text, Text) -> Html ()
--ppHtmlModificationTableRow = undefined
ppHtmlModificationTableRow (_, colIdx, row1, row2) = tr_ (do td_ (toHtml colIdx)
                                                             td_ (toHtml row1)
                                                             td_ (toHtml row2))

cssStyle :: String
cssStyle = "table.gridtable {\
\font-family: verdana,arial,sans-serif;\
\font-size:11px;\
\color:#333333;\
\border-width: 1px;\
\border-color: #666666;\
\border-collapse: collapse;\
\}\
\table.gridtable th {\
\border-width: 1px;\
\padding: 8px;\
\border-style: solid;\
\border-color: #666666;\
\background-color: #dedede;\
\}\
\table.gridtable td {\
\border-width: 1px;\
\padding: 8px;\
\border-style: solid;\
\border-color: #666666;\
\background-color: #ffffff;\
\}"

makeTable :: [IndexedRecord] -> IndexedCsv -> Html ()
makeTable recs csv =
  table_ [class_ "gridtable"] (do
    thead_ (
      tr_ (mapM_ (td_ . toHtml)  (header csv)));
    mapM_ makeRow recs)

makeRow :: IndexedRecord -> Html ()
makeRow rec = tr_ (mapM_ (td_ . toHtml) (snd rec))

ppChangeset :: Changeset -> String
ppChangeset changeset = "Adds:\n" <> ppAdds (adds changeset) <>
  "Deletes:\n" <> ppAdds (deletes changeset) <>
  "Modifications: \n" <>  unpack (ppModifications (modifications changeset)) <>
    "Columns Added: " <> show (columnsAdded changeset) <> "\n" <>
    "Columns Deleted: " <> show (columnsDeleted changeset)

ppAdds :: [IndexedRecord] -> String
ppAdds recs = "[\n" <> Data.List.foldl' (\acc rec -> acc <> "\t" <> show rec <> "\n") "" recs <>  "]\n"

ppModifications :: [(RowIndex, ColumnIndex, Text, Text)] -> Text
ppModifications mods = "[\n" <> intercalate ",\n" (fmap ppModification (groupBy (\(idx1, _, _, _) (idx2, _, _, _) -> idx1==idx2) mods)) <> "\n]\n"

-- print all modifications with a matching rowindex
ppModification :: [(RowIndex, ColumnIndex, Text, Text)] -> Text
ppModification [] = ""
ppModification ((idx, colIdx, row1, row2):rst) = pack "\t(" <> pack (show idx) <> pack ", " <> intercalate ", " (fmap ppModValues ((idx,colIdx,row1,row2):rst)) <> ")"

ppModValues :: (RowIndex, ColumnIndex, Text, Text) -> Text
ppModValues (_, colIdx, row1, row2) = pack (show colIdx <> ": " <> show row1 <> " -> " <> show row2)
