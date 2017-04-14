--import           CsvDiffImpl
--import           Test.QuickCheck

main :: IO ()
main = putStrLn "Test suite not yet implemented"
--main = quickCheck prop_columnAdditionsDeletions

-- prop_columnAdditionsDeletions :: IndexedCsv -> IndexedCsv -> Bool
-- prop_columnAdditionsDeletions csv1 csv2 = findDeletedColumns csv1 csv2 == findAddedColumns csv2 csv1
--
-- prop_additionsDeletions :: IndexedCsv -> IndexedCsv -> Bool
-- prop_additionsDeletions csv1 csv2 = findDeletes csv1 csv2 == findAdds csv2 csv1
