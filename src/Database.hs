module Database where

import qualified Data.Map
import Data.Maybe (fromJust, isNothing)
import Data.List (elemIndex, elemIndices)
import qualified ExpressionParser as Exp
import qualified Text.Parsec.Error

import qualified Funcs as F
import qualified CreateParser as CP

-- for debugging only. To be removed once completed - called from main right now!
sampleCommands = do
    print "======================================="
    
    let my_db = newDatabase "my_db 1" Data.Map.empty
    -- print $ my_db

    let my_db_1 = addNewTable "table 1" my_db
    -- print $ my_db_1

    let my_db = my_db_1
    let my_db_1 = addNewTable "table 2" my_db
    -- print $ my_db_1

    let my_db = my_db_1
    let contains = containsTable "table 1" my_db
    -- print $ contains

    let my_db_1 = addColumn "Name" STRING my_db "table 1"
    let my_db = my_db_1
    let my_db_1 = addColumn "Phone_number" INT my_db "table 1"
    let my_db = addColumn "BoolCol" BOOL my_db_1 "table 1"
    -- print my_db_1

    -- let my_db = my_db_1
    -- let my_db_1 = insertOne "23" INT my_db "table 1" "column 1 for table 1"
    -- print my_db_1
    
    -- let my_db = my_db_1
    -- let my_db_1 = insertOne "45" INT my_db "table 1" "column 1 for table 1"
    -- print my_db_1

    let my_db_1 = insert ["Name", "Phone_number", "BoolCol"] ["Martha", "12345", "False"] [STRING, INT, BOOL] my_db "table 1"
    -- print my_db_1
    print(my_db_1)
    let my_db = my_db_1
    let my_db_1 = insert ["BoolCol", "Name", "Phone_number"] ["True", "Stewart", "78910"] [BOOL, STRING, INT] my_db "table 1"
    print(my_db_1)

    let m = F.parseWithWSEof (Exp.valueExpr []) "BoolCol"
    let n = F.parseWithWSEof (Exp.valueExpr []) "Phone_number > 0"
    let o = F.parseWithWSEof (Exp.valueExpr []) "Phone_number = 12345"
    print m
    let x = find m my_db_1 "table 1"
    print x
    print n
    let x = find n my_db_1 "table 1"
    print x
    print o
    let x = find o my_db_1 "table 1"
    print x

    -- print my_db_1


    -- let my_db = my_db_1
    -- let my_db_1 = insert ["Name", "Phone number"] ["Stewart", "12321"] [STRING, INT] my_db "table 1"

    -- let x = find (\value -> value == "Stewart") my_db_1 "table 1" "Name"
    -- print(x)
    -- let my_db = deleteEntryAtIndex 3 my_db_1 "table 1"
    -- print(my_db)
    
    -- print my_db_1

    return 0


-- I understand that the code in this file is terrible. That's because of my f*cked up data-structure. If I have sufficient time to rebuild the structure, I will do it. But it's gonna be a lot of work.

data Column = Column {
    cName :: String,
    cDatatype :: Datatype, -- For Num => Num, String => String, Bool => Bool
    cValues :: [String]
} deriving (Show, Eq)

-- Returns new column with name, datatype and values
newColumn :: String -> Datatype -> [String] -> Maybe Column
newColumn name datatype values = Just Column {
    cName = name,
    cDatatype = datatype,
    cValues = values
}

data Table = Table {
    tName :: String,
    tColNameList :: [String],
    tColumns :: Data.Map.Map String Column
} deriving (Show, Eq)

-- Returns new table with name and columns
newTable :: String -> ([String], Data.Map.Map String Column) -> Maybe Table
newTable name (colNameList, columns) = Just Table {
    tName = name,
    tColNameList = colNameList,
    tColumns = columns
}
    
data Database = Database {
    dName :: String,
    dTables :: Data.Map.Map String Table
} deriving (Show, Eq)

-- Returns new database with name and tables
newDatabase :: String -> Data.Map.Map String Table -> Maybe Database
newDatabase name tables = Just Database {
    dName = name,
    dTables = tables
}

-- For datatype in a column
data Datatype = INT | STRING | BOOL deriving (Show, Eq)

-- True if database contains table named tableName
containsTable :: String -> Maybe Database -> Bool
containsTable tableName database = not (isNothing $ getTable tableName database)

-- True if database -> tableName contains a column named columnName
containsColumn :: String -> Maybe Database -> String -> Bool
containsColumn columnName database tableName = elem columnName (tColNameList $ fromJust $ getTable tableName database)

-- Returns table from database with name tableName
getTable :: String -> Maybe Database -> Maybe Table
getTable tableName database
    | isNothing database = Nothing
    | otherwise = Data.Map.lookup tableName $ dTables $ fromJust database

count :: Maybe Table -> Int
count table
    | isNothing table || Data.Map.size (tColumns $ fromJust table) == 0 = 0
    | otherwise = let firstCol = snd $ Data.Map.elemAt 0 (tColumns $ fromJust table)
        in length (cValues firstCol)


-- Returns column with columnName in database -> tableName
getColumn :: String -> Maybe Table -> Maybe Column
getColumn columnName table
    | isNothing table = Nothing
    | otherwise = Data.Map.lookup columnName (tColumns $ fromJust table)

-- Adds a table to database with tableName
addNewTable :: String -> Maybe Database -> Maybe Database
addNewTable tableName database
    | isNothing database = Nothing
    | otherwise = Just Database {
        dName = dName $ fromJust database,
        dTables = Data.Map.insert tableName (fromJust $ newTable tableName ([],Data.Map.empty)) (dTables $ fromJust database)
    }

-- Adds column to table object with name and datatype
addColumnToTable :: String -> Datatype -> Table -> Table
addColumnToTable columnName datatype table = fromJust $ newTable (tName table)
        (columnName:(tColNameList table), (Data.Map.insert columnName (fromJust $ newColumn columnName datatype []) (tColumns table)))

-- Adds a column to database -> tableName with name=columnName, datatype=datatype
addColumn :: String -> Datatype -> Maybe Database -> String -> Maybe Database
addColumn columnName datatype database tableName
    | isNothing database || not (containsTable tableName database) = Nothing
    | otherwise = newDatabase (dName $ fromJust database)
            (Data.Map.adjust (addColumnToTable columnName datatype) tableName $ dTables $ fromJust database)

-- Input value and datatype to be inserted in a single column
insertOne :: String -> Datatype -> Maybe Database -> String -> String -> Maybe Database
insertOne value datatype database tableName columnName
    | isNothing database || not (containsTable tableName database) || not (containsColumn columnName database tableName)
        = database -- Invalid table name or col name
    | datatype /= cDatatype (fromJust $ getColumn columnName $ getTable tableName database )= database -- Datatypes dont match
    | otherwise = newDatabase (dName $ fromJust database) (Data.Map.adjust f tableName $ dTables $ fromJust database)
    where f = (\table -> fromJust $ newTable (tName table) (tColNameList table, (Data.Map.adjust g columnName $ tColumns table)))
          g = (\column -> fromJust $ newColumn (cName column) (cDatatype column) ((cValues column) ++ [value]))

isValidDatatype :: Maybe Table -> [String] -> [Datatype] -> Bool
isValidDatatype t colList typeList
    | isNothing t || (colList == [] && typeList == [] )= True
    | colList == [] || typeList == [] = False
    | otherwise = 
        let (dType:dTypes) = typeList
            (col:cols) = colList
        in (dType == cDatatype (fromJust $ getColumn col t)) && (isValidDatatype t cols dTypes)
          

-- Input - list of colNames <-> corresponding list of values <-> corresponding list of datatypes
insert :: [String] -> [String] -> [Datatype] -> Maybe Database -> String -> Maybe Database
insert columnNames values datatypes db tableName 
    | length values /= length datatypes || not (isValidDatatype (getTable tableName db) columnNames datatypes) || length columnNames /= length values = db --Mismatch in length of names, values and datatypes
    | length values == 0 = db -- Base case
    | otherwise = do
        let v = head values; vs = tail values
        let d = head datatypes; ds = tail datatypes
        let col = head columnNames; cols = tail columnNames
        let new_db = insertOne v d db tableName col
        insert cols vs ds new_db tableName

insertDefault :: [String] -> Maybe Database -> String -> Maybe Database
insertDefault values db tableName
    | not datatypeOK colNameList values table = db
    | otherwise =
            colNameList = tColNameList $ fromJust table
        in insert colNameList values (map (\name -> cDatatype $ fromJust $ getColumn name table) colNameList) db tableName
    where table = getTable tableName db
          colNameList = if isNothing table then [] else tColNameList $ fromJust table
-- Utility functions for find*
getColList table db= Data.Map.elems (tColumns $ fromJust $ getTable table db)

-- Finds entry at index in table and returns as [tuples] where tuple = (col name, col type, col value)
findEntryAtIndex :: String -> Maybe Database -> Int -> [(String, Datatype, String)]
findEntryAtIndex tableName db index = [ cell | col <- getColList tableName db,
    let cell = (cName col, cDatatype col, (cValues col) !! index)]

litValue :: String -> Datatype -> Exp.ValueExpr
litValue value datatype
    | datatype == INT = Exp.NumLit (read value :: Integer)
    | datatype == BOOL = if value == "True" then Exp.BoolLit True else Exp.BoolLit False
    | datatype == STRING = Exp.StringLit value

-- Finds all entries in (db -> table) with value, datype and returns as [entries] where entry=[tuples] where tuple = (col name, col type, col value)
find :: Either Text.Parsec.Error.ParseError Exp.ValueExpr -> Maybe Database -> String -> [[(String, Datatype, String)]]
find (Right condition) db tableName
    | otherwise =
        let table = getTable tableName db
            nEntries = count table
            columns = Data.Map.elems $ tColumns $ fromJust table 
            indices = [n | n <- [0..nEntries-1], let map = Data.Map.fromList [(colName, value) | col <- columns, let colName = cName col, let value = litValue ((cValues col) !! n) (cDatatype col)], Exp.evaluateExpr2 map condition]
        in [findEntryAtIndex tableName db n | n <- indices]

deleteFromList :: Int -> [String] -> [String]
deleteFromList index xs = (take index xs) ++ reverse(take (length xs - index - 1) (reverse xs))

-- Delete an entry from the table
deleteEntryAtIndex :: Int -> Maybe Database -> String -> Maybe Database
deleteEntryAtIndex index db tableName
    | isNothing db = Nothing
    | otherwise = do
        let my_db = fromJust db
        newDatabase (dName my_db) (Data.Map.adjust f tableName $ dTables my_db)
    where f = \table -> (fromJust $ newTable (tName table) (tColNameList table, (Data.Map.fromList new_cols)))
          cols = Data.Map.elems $ tColumns $ fromJust $ getTable tableName db
          new_cols = [(cName col, fromJust $ newColumn (cName col) (cDatatype col) (deleteFromList index $ cValues col)) | col <- cols]