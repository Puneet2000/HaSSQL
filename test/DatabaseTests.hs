module DatabaseTests where
import Test.HUnit as H
import qualified Database as DB
import qualified ExpressionParser as Exp
import qualified Funcs as F
import Data.Maybe(fromJust)
import qualified Data.Map as M
import Data.List (elem)

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
    let addTests = H.TestList [
            H.TestLabel "addNewTable" testAddNewTable,
            H.TestLabel "addColumnToTable" testAddColumnToTable,
            H.TestLabel "addColumn" testAddColumn]
    let findTests = H.TestList [
            H.TestLabel "find1" testFind,
            H.TestLabel "find2" testFind2]
    let insertTests = H.TestList [
            H.TestLabel "insertOne" testInsertOne,
            H.TestLabel "insert" testInsert,
            H.TestLabel "insertDefault" testInsertDefault]
    let deleteTests = H.TestList[
            H.TestLabel "delete" testDelete]
    let orderByTests = H.TestList[
            H.TestLabel "orderBy" testOrderBy]
    let selectTests = H.TestList[
            H.TestLabel "select" testSelect]

    runTestTT newTests
    runTestTT containsTests
    runTestTT getTests
    runTestTT addTests
    runTestTT findTests
    runTestTT insertTests
    runTestTT deleteTests
    runTestTT orderByTests
    runTestTT selectTests

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
        myNewTable = fromJust $ DB.newTable "testTable" (["testCol"], myCols) [0,1]
    in H.TestCase (do
        H.assertEqual "Name not set properly" "testTable" $ DB.tName myNewTable
        H.assertEqual "Cols not set properly" myCols $ DB.tColumns myNewTable
    )

testNewDatabase :: H.Test
testNewDatabase =
    let myCols = M.singleton "testCol" $ fromJust $ DB.newColumn "testCol" DB.INT ["testVal1", "testVal2"]
        myTables = M.singleton "testTable" $ fromJust $ DB.newTable "testTable" (["testCol"], myCols) [0,1]
        myNewDB = fromJust $ DB.newDatabase "testDB" myTables
    in H.TestCase (do
        H.assertEqual "Name not set properly" "testDB" $ DB.dName myNewDB
        H.assertEqual "Tables not set properly" myTables $ DB.dTables myNewDB
    )

sampleIntCol = DB.Column {
    DB.cName = "sampleIntCol", DB.cDatatype = DB.INT, DB.cValues = ["123", "987"]
}
sampleStringCol = DB.Column {
    DB.cName = "sampleStringCol", DB.cDatatype = DB.STRING, DB.cValues = ["testVal1s", "testVal2s"]
}
sampleColMap = M.fromList [("sampleIntCol", sampleIntCol),("sampleStringCol", sampleStringCol)]
sampleTableOne = DB.Table {
    DB.tName = "sampleTableOne", DB.tColumns = sampleColMap, DB.tColNameList = ["sampleIntCol", "sampleStringCol"], DB.tPKeys = [0,1]
}
sampleTableTwo = DB.Table {
    DB.tName = "sampleTableTwo", DB.tColumns = sampleColMap, DB.tColNameList = ["sampleIntCol", "sampleStringCol"], DB.tPKeys = [0,1]
}
sampleDB = Just DB.Database {
    DB.dName = "testDB", DB.dTables = M.fromList [("sampleTableOne", sampleTableOne), ("sampleTableTwo", sampleTableTwo)]
}

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

testCount :: H.Test
testCount = H.TestCase(do
        H.assertEqual "count does not work properly" 2 (DB.count (Just sampleTableOne))
        H.assertEqual "count /= 0 for Nothing" 0 (DB.count Nothing)
        H.assertEqual "count /= 0 even if there are no columns" 0 (DB.count $ DB.newTable "myNewTable" ([], M.fromList []) [])
    )

testGetColumn :: H.Test
testGetColumn = 
    let receivedColumn = DB.getColumn "sampleStringCol" $ DB.getTable "sampleTableOne" sampleDB
        nothingColumn = DB.getColumn "InvalidColumn" $ DB.getTable "sampleTableOne" sampleDB
    in H.TestCase(do
        H.assertEqual "getColumn for valid column does not work properly" (Just sampleStringCol) receivedColumn
        H.assertEqual "getColumn for Nothing does not work properly" Nothing nothingColumn
    )

testAddNewTable :: H.Test
testAddNewTable =
    let myNewDB = DB.addNewTable "myNewTable" sampleDB
        searchedTable = DB.getTable "myNewTable" myNewDB
    in H.TestCase (do
        H.assertEqual "addNewTable not adding a new Table" (DB.newTable "myNewTable" ([], M.empty) []) searchedTable
    )

testAddColumnToTable :: H.Test
testAddColumnToTable = 
    let myNewTable = DB.addColumnToTable "myNewCol" DB.BOOL sampleTableOne
        searchedCol = DB.getColumn "myNewCol" (Just myNewTable)
    in H.TestCase (do
        H.assertEqual "addColumnToTable does not add an empty column to table" (DB.newColumn "myNewCol" DB.BOOL []) searchedCol
    )

testAddColumn :: H.Test
testAddColumn =
    let myNewDB = DB.addColumn "myNewCol" DB.BOOL sampleDB "sampleTableOne"
        addedCol = DB.newColumn "myNewCol" DB.BOOL []
        foundCol = DB.getColumn "myNewCol" $ DB.getTable "sampleTableOne" myNewDB
    in H.TestCase (do
        H.assertEqual "addColumn does not work properly" addedCol foundCol
    )

testInsertOne :: H.Test
testInsertOne = 
    let myNewDB = DB.insertOne "newVali" DB.INT sampleDB "sampleTableOne" "sampleIntCol"
        prevLength = DB.count $ DB.getTable "sampleTableOne" sampleDB 
        newLength = DB.count $ DB.getTable "sampleTableOne" myNewDB 
    in H.TestCase (do
        H.assertEqual "Length did not increase by one on insertOne" (prevLength + 1) newLength
    )

testFind :: H.Test
testFind =
    let condition = F.parseWithWSEof (Exp.valueExpr []) "sampleIntCol > 200"
        foundVals = DB.find condition sampleDB "sampleTableOne"
        expectedVals = [
            [(1, "sampleIntCol", DB.INT, "987"), (1, "sampleStringCol", DB.STRING, "testVal2s")]]
        in H.TestCase (do
            H.assertBool "find does not work properly" (all (\val -> elem val expectedVals) foundVals)
        )

testFind2 :: H.Test
testFind2 =
    let condition = F.parseWithWSEof (Exp.valueExpr []) "sampleIntCol > 0"
        foundVals = DB.find condition sampleDB "sampleTableOne"
        expectedVals = [
            [(1, "sampleIntCol", DB.INT, "987"), (1, "sampleStringCol", DB.STRING, "testVal2s")],
            [(0, "sampleIntCol", DB.INT, "123"), (0, "sampleStringCol", DB.STRING, "testVal1s")]]
    in H.TestCase (do
        H.assertBool "find does not work properly" (all (\val -> elem val expectedVals ) foundVals)
    )

testInsert :: H.Test
testInsert =
    let myNewDB = DB.insert ["sampleIntCol", "sampleStringCol"] ["99", "newString"] [DB.INT, DB.STRING] sampleDB "sampleTableOne"
        condition = F.parseWithWSEof (Exp.valueExpr []) "sampleIntCol = 99"
        search99 = DB.find condition myNewDB "sampleTableOne"
        expected99 = [
            [(2, "sampleIntCol", DB.INT, "99"), (2, "sampleStringCol", DB.STRING, "newString")]]
    in H.TestCase (do
        H.assertBool "insert is not working properly!" (all (\val -> elem val expected99) search99)
    )

testInsertDefault :: H.Test
testInsertDefault =
    let myNewDB = DB.insertDefault ["99", "newString"] sampleDB "sampleTableOne"
        condition = F.parseWithWSEof (Exp.valueExpr []) "sampleIntCol = 99"
        search99 = DB.find condition myNewDB "sampleTableOne"
        expected99 = [
            [(2, "sampleIntCol", DB.INT, "99"), (2, "sampleStringCol", DB.STRING, "newString")]]
    in H.TestCase (do
        H.assertBool "insert is not working properly!" (all (\val -> elem val expected99) search99)
    )

testDelete :: H.Test
testDelete = 
    let strConditions = ["sampleIntCol = 99", "sampleIntCol > 200", "sampleIntCol > 0"]
        conditions = [F.parseWithWSEof (Exp.valueExpr []) condition | condition <- strConditions]
        myNewDBs = [DB.delete condition sampleDB "sampleTableOne" | condition <- conditions]
        finds = [DB.find condition myNewDB "sampleTableOne" | n <- [0..length strConditions-1], let condition = conditions !! n, let myNewDB = myNewDBs !! n]
    in H.TestCase(
        H.assertBool "delete does not work properly" (all (\finds -> finds == []) finds)
    )

testInputList = [
    [(100, "a", DB.INT, "1"), (100, "b", DB.INT, "3"), (100, "c", DB.INT, "2")],
    [(102, "a", DB.INT, "3"), (102, "b", DB.INT, "2"), (102, "c", DB.INT, "3")],
    [(108, "a", DB.INT, "2"), (108, "b", DB.INT, "2"), (108, "c", DB.INT, "2")]]

testOrderBy :: H.Test
testOrderBy =
    let expr = F.parseWithWSEof (Exp.valueExpr []) "(a*a) - (b*b)" 
        input = testInputList
        output = DB.orderBy expr input
        expOutput = [
            [(100, "a", DB.INT, "1"), (100, "b", DB.INT, "3"), (100, "c", DB.INT, "2")],
            [(108, "a", DB.INT, "2"), (108, "b", DB.INT, "2"), (108, "c", DB.INT, "2")],
            [(102, "a", DB.INT, "3"), (102, "b", DB.INT, "2"), (102, "c", DB.INT, "3")]]
    in H.TestCase (do
        H.assertBool "orderBy does not work properly" (all (\i -> output !! i == expOutput !! i) [0..length output-1])
    )

testSelect :: H.Test
testSelect =
    let input = testInputList
        output1 = DB.select [("a", "Testa"), ("c", "Testc")] input
        output2 = DB.select [] input
        exp1 = [
            [(100, "Testa", DB.INT, "1"), (100, "Testc", DB.INT, "2")],
            [(102, "Testa", DB.INT, "3"), (102, "Testc", DB.INT, "3")],
            [(108, "Testa", DB.INT, "2"), (108, "Testc", DB.INT, "2")]]
        exp2 = testInputList
    in H.TestCase (do
        H.assertBool "select does not work properly" (all (\val -> elem val exp1) output1)
        H.assertBool "select does not handle empty case properly" (all (\val -> elem val exp2) output2)
    )