module DatabaseTests where
import Test.HUnit as H
import Database as DB
import Data.Maybe(fromJust)
import Data.Map as M

databaseTests :: IO H.Counts
databaseTests = do
    let newTests = H.TestList [
            H.TestLabel "newColumn" testNewColumn,
            H.TestLabel "newTable" testNewTable,
            H.TestLabel "newDatabase" testNewDatabase ]
    let containsTests = H.TestList [
            H.TestLabel "conatinsTable" testContainsTable,
            H.TestLabel "containsColumn" testContainsColumn]
    let getTests = H.TestList [
            H.TestLabel "getTable" testGetTable,
            H.TestLabel "getColumn" testGetColumn]
    runTestTT newTests
    runTestTT containsTests
    runTestTT getTests

testNewColumn :: H.Test
testNewColumn =
    let myNewCol = fromJust $ DB.newColumn "testCol" DB.INT ["testVal1", "testVal2"]
    in H.TestCase (do
        H.assertEqual "Name not set properly" "testCol" (DB.cName myNewCol)
        H.assertEqual "Datatype not set properly" DB.INT (DB.cDatatype myNewCol)
        H.assertEqual "Values not set properly" ["testVal1", "testVal2"] (DB.cValues myNewCol)
    )

testNewTable :: H.Test
testNewTable =
    let myCols = M.singleton "testCol" $ fromJust $ DB.newColumn "testCol" DB.INT ["testVal1", "testVal2"]
        myNewTable = fromJust $ DB.newTable "testTable" myCols
    in H.TestCase (do
        H.assertEqual "Name not set properly" "testTable" $ DB.tName myNewTable
        H.assertEqual "Cols not set properly" myCols $ DB.tColumns myNewTable
    )

testNewDatabase :: H.Test
testNewDatabase =
    let myCols = M.singleton "testCol" $ fromJust $ DB.newColumn "testCol" DB.INT ["testVal1", "testVal2"]
        myTables = M.singleton "testTable" $ fromJust $ DB.newTable "testTable" myCols
        myNewDB = fromJust $ DB.newDatabase "testDB" myTables
    in H.TestCase (do
        H.assertEqual "Name not set properly" "testDB" $ DB.dName myNewDB
        H.assertEqual "Tables not set properly" myTables $ DB.dTables myNewDB
    )
    
sampleIntCol = fromJust $ DB.newColumn "sampleIntCol" DB.INT ["testVal1i", "testVal2i"]
sampleStringCol = fromJust $ DB.newColumn "sampleStringCol" DB.STRING ["testVal1s", "testVal2s"]
sampleColMap = M.insert "sampleStringCol" sampleStringCol $ M.singleton "sampleIntCol" sampleIntCol
sampleTableOne = fromJust $ DB.newTable "sampleTableOne" sampleColMap
sampleTableTwo = fromJust $ DB.newTable "sampleTableTwo" sampleColMap
sampleTableMap = M.insert "sampleTableOne" sampleTableOne $ M.singleton "sampleTableTwo" sampleTableTwo
sampleDB = DB.newDatabase "testDB" sampleTableMap

testContainsTable :: H.Test
testContainsTable = 
    let contains = DB.containsTable "sampleTableOne" sampleDB
        doesNotContain = DB.containsTable "Invalid Table" sampleDB
    in H.TestCase $ H.assertBool "containsTable does not work properly" (contains && not doesNotContain)

testContainsColumn :: H.Test
testContainsColumn = 
    let contains = DB.containsColumn "sampleStringCol" sampleDB "sampleTableOne"
        doesNotContain = DB.containsColumn "testInvalidCol" sampleDB "sampleTableOne"
    in H.TestCase $ H.assertBool "containsColumn does not work properly" (contains && not doesNotContain)

testGetTable :: H.Test
testGetTable =
    let receivedTable = DB.getTable "sampleTableOne" sampleDB
        nothingTable = DB.getTable "InvalidTable" sampleDB
    in H.TestCase (do
        H.assertEqual "getTable for valid table does not work properly" (Just sampleTableOne) receivedTable
        H.assertEqual "getTable for Nothing does not work properly" Nothing nothingTable
    )

testGetColumn :: H.Test
testGetColumn = 
    let receivedColumn = DB.getColumn "sampleStringCol" sampleDB "sampleTableOne"
        nothingColumn = DB.getColumn "InvalidColumn" sampleDB "sampleTableOne"
    in H.TestCase(do
        H.assertEqual "getColumn for valid column does not work properly" (Just sampleStringCol) receivedColumn
        H.assertEqual "getColumn for Nothing does not work properly" Nothing nothingColumn
    )

