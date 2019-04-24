module Database where

import qualified Data.Map
import Data.Maybe (fromJust, isNothing)
import Data.List (elemIndex, elemIndices)
import Data.Sort(sortBy)
-- import Data.List.Key (sort)
import qualified ExpressionParser as Exp
import qualified Text.Parsec.Error

import qualified Funcs as F


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
    -- print(my_db_1)
    let my_db = my_db_1
    let my_db_1 = insert ["BoolCol", "Name", "Phone_number"] ["True", "Stewart", "78910"] [BOOL, STRING, INT] my_db "table 1"
    -- print(my_db_1)

    let m = F.parseWithWSEof (Exp.valueExpr []) "BoolCol"
    let n = F.parseWithWSEof (Exp.valueExpr []) "Phone_number > 0"
    let o = F.parseWithWSEof (Exp.valueExpr []) "Phone_number = 12345"
    let p = F.parseWithWSEof (Exp.valueExpr []) "-1 * Phone_number"
    -- print m
    let x = delete n my_db_1 "table 1"
    print("Y=-------------------->")
    print x
    -- print n
    let x = find n my_db_1 "table 1"
    -- print x
    let y = select [("BoolCol", "IsFemale"), ("Phone_number", "home_number")] $ orderBy p x
    -- print y
    -- print o
    let x = find o my_db_1 "table 1"
    -- print x

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

-- | 'Column' stores a column in a table as a list of its values along with the datatype and name of column
data Column = Column {
    cName :: String,
    cDatatype :: Datatype, -- For Num => Num, String => String, Bool => Bool
    cValues :: [String]
} deriving (Show, Eq)

-- |'newColumn' Returns new column (Maybe Column)
-- First argument is name of column
-- Second argument is datatype
-- Third argument is list of values
newColumn :: String -> Datatype -> [String] -> Maybe Column
newColumn name datatype values = Just Column {
    cName = name,
    cDatatype = datatype,
    cValues = values
}

-- | 'Table' stores a map of columns (key is column Name) with table name and list of names of columns
data Table = Table {
    tName :: String,
    tColNameList :: [String],
    tColumns :: Data.Map.Map String Column
} deriving (Show, Eq)

-- | 'newTable' returns new table (Maybe Table)
-- First argument is table name
-- Second argument is (list of colNames, Map of those colnames -> Columns)
newTable :: String -> ([String], Data.Map.Map String Column) -> Maybe Table
newTable name (colNameList, columns) = Just Table {
    tName = name,
    tColNameList = colNameList,
    tColumns = columns
}

-- | 'Database' stores a database with a name and map of tables indexed by their names
data Database = Database {
    dName :: String,
    dTables :: Data.Map.Map String Table
} deriving (Show, Eq)

-- | 'newDatabase' returns new database (Maybe Database)
-- First argument is datbase name
-- Second argument map of tables indexed by their names
newDatabase :: String -> Data.Map.Map String Table -> Maybe Database
newDatabase name tables = Just Database {
    dName = name,
    dTables = tables
}

-- | 'Datatype' is for datatype in a column
data Datatype = INT | STRING | BOOL deriving (Show, Eq)

-- | 'containsTable' is True if database contains table, o/w False
-- First argument is tableName
-- Second argument is database object (Maybe database)
containsTable :: String -> Maybe Database -> Bool
containsTable tableName database = not (isNothing $ getTable tableName database)

-- | 'containsColumn' returns True if table within database contains a column, o/w false
-- First argument is column name
-- Second argument is database (Maybe Database)
-- Third argument is table name
containsColumn :: String -> Maybe Database -> String -> Bool
containsColumn columnName database tableName = elem columnName (tColNameList $ fromJust $ getTable tableName database)

-- | 'getTable' returns table from database (Maybe Table)
-- First argument is table name
-- Second argument is database (Maybe Database)
getTable :: String -> Maybe Database -> Maybe Table
getTable tableName database
    | isNothing database = Nothing
    | otherwise = Data.Map.lookup tableName $ dTables $ fromJust database

-- | 'count' returns the number of entries in a table
-- First argument is table (Maybe Table)
count :: Maybe Table -> Int
count table
    | isNothing table || Data.Map.size (tColumns $ fromJust table) == 0 = 0
    | otherwise = let firstCol = snd $ Data.Map.elemAt 0 (tColumns $ fromJust table)
        in length (cValues firstCol)


-- | 'getColumn' returns column in specific table of a database (Maybe Column)
-- First argument is name of column (String)
-- Second argument is table (Maybe Table)
getColumn :: String -> Maybe Table -> Maybe Column
getColumn columnName table
    | isNothing table = Nothing
    | otherwise = Data.Map.lookup columnName (tColumns $ fromJust table)

-- | 'addNewTable' adds an empty table to database and returns new database (Maybe Database)
-- First argument is tableName (String)
-- Second argument is database (Maybe Database)
addNewTable :: String -> Maybe Database -> Maybe Database
addNewTable tableName database
    | isNothing database = Nothing
    | otherwise = Just Database {
        dName = dName $ fromJust database,
        dTables = Data.Map.insert tableName (fromJust $ newTable tableName ([],Data.Map.empty)) (dTables $ fromJust database)
    }

-- | 'addColumnToTable' adds a column to a table and returns new table (Table)
-- First argument is column name (String)
-- Second argument is column datatype (Datatype)
-- Third argumenr is old table (Table)
addColumnToTable :: String -> Datatype -> Table -> Table
addColumnToTable columnName datatype table = fromJust $ newTable (tName table)
        ((tColNameList table) ++ [columnName], (Data.Map.insert columnName (fromJust $ newColumn columnName datatype []) (tColumns table)))

-- | 'addColumn' adds a column to specific table in database and returns new database (Maybe Database)
-- First argument is column name (String)
-- Second argumenr is column datatype (Datatype)
-- Third argumenr is database (Maybe Database)
-- Fourth argumenr is table name (String)
addColumn :: String -> Datatype -> Maybe Database -> String -> Maybe Database
addColumn columnName datatype database tableName
    | isNothing database || not (containsTable tableName database) = database
    | otherwise = newDatabase (dName $ fromJust database)
            (Data.Map.adjust (addColumnToTable columnName datatype) tableName $ dTables $ fromJust database)

-- | 'insertOne' inserts one entry in one column of one table in a database
-- First argument is value (String)
-- Second argument is it's datatype (Datatype)
-- Third argument is old database (Maybe Database)
-- Fourth argument is table name (String)
-- Fifth argumenr is column name (String)
insertOne :: String -> Datatype -> Maybe Database -> String -> String -> Maybe Database
insertOne value datatype database tableName columnName
    | isNothing database || not (containsTable tableName database) || not (containsColumn columnName database tableName)
        = database -- Invalid table name or col name
    | datatype /= cDatatype (fromJust $ getColumn columnName $ getTable tableName database )= database -- Datatypes dont match
    | otherwise = newDatabase (dName $ fromJust database) (Data.Map.adjust f tableName $ dTables $ fromJust database)
    where f = (\table -> fromJust $ newTable (tName table) (tColNameList table, (Data.Map.adjust g columnName $ tColumns table)))
          g = (\column -> fromJust $ newColumn (cName column) (cDatatype column) ((cValues column) ++ [value]))


-- | 'isValidDatatype' checks if datatypes corresponding to column names list are in sync with the table in database or not. Returns true if they both conform
-- First argumenr is table (Maybe Table)
-- Second argument is list of column names [String]
-- Third argument is list of datatypes [Datatype]
isValidDatatype :: Maybe Table -> [String] -> [Datatype] -> Bool
isValidDatatype t colList typeList
    | isNothing t || (colList == [] && typeList == [] )= True
    | colList == [] || typeList == [] = False
    | otherwise = 
        let (dType:dTypes) = typeList
            (col:cols) = colList
        in (dType == cDatatype (fromJust $ getColumn col t)) && (isValidDatatype t cols dTypes)
          

-- | 'insert' inserts an entry (row) in database -> table and returns new database
-- First argumenr is list of colNames [String]
-- Second argument is corresponding list of values [String]
-- Third argumenr is corresponding list of datatypes [Datatype]
-- Fourth argument old database (Maybe Database)
-- Fifth argument is table name (String)
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


-- | 'insertDefault' inserts an entry in the default column order. This does not check for datatype as of now.
-- First argument is list of values
-- Second argument is old database
-- Third argument is tableName
insertDefault :: [String] -> Maybe Database -> String -> Maybe Database
insertDefault values db tableName
    -- | not datatypeOK colNameList values table = db
    | otherwise =
        let colNameList = tColNameList $ fromJust table
        in insert colNameList values (map (\name -> cDatatype $ fromJust $ getColumn name table) colNameList) db tableName
    where table = getTable tableName db
          colNameList = if isNothing table then [] else tColNameList $ fromJust table

-- | 'getColList' is a Utility function for find*
getColList table db= Data.Map.elems (tColumns $ fromJust $ getTable table db)

-- | 'findEntryAtIndex' finds entry at index in table and returns as [tuples] where tuple = (col name, col type, col value)
-- First argument is tableName
-- Second argument is database
-- Third argument is index
findEntryAtIndex :: String -> Maybe Database -> Int -> [(String, Datatype, String)]
findEntryAtIndex tableName db index = [ cell | col <- getColList tableName db,
    let cell = (cName col, cDatatype col, (cValues col) !! index)]

-- 'litValue' is a utility function to support evaluation of ValueExpr on columns (which are stored as String)
litValue :: String -> Datatype -> Exp.ValueExpr
litValue value datatype
    | datatype == INT = Exp.NumLit (read value :: Integer)
    | datatype == BOOL = if value == "True" then Exp.BoolLit True else Exp.BoolLit False
    | datatype == STRING = Exp.StringLit value

-- | 'find' finds all entries in (db -> table) with value, datype and returns as [entries] where entry=[tuples] where tuple = (col name, col type, col value)
-- First argument is value (Right) ValueExpr that evaluates to true or false
-- Second argument is database (Maybe Database)
-- Third argument is table name
find :: Either Text.Parsec.Error.ParseError Exp.ValueExpr -> Maybe Database -> String -> [[(String, Datatype, String)]]
find (Right condition) db tableName
    | isNothing db || not (containsTable tableName db) = []
    | otherwise =
        let table = getTable tableName db
            nEntries = count table
            columns = Data.Map.elems $ tColumns $ fromJust table 
            indices = [n | n <- [0..nEntries-1], let map = Data.Map.fromList [(colName, value) | col <- columns, let colName = cName col, let value = litValue ((cValues col) !! n) (cDatatype col)], Exp.evaluateExpr2 map condition]
        in [findEntryAtIndex tableName db n | n <- indices]

-- | 'deleteFromList' is a utility function to delete an entry from a list
deleteFromList :: Int -> [String] -> [String]
deleteFromList index xs = (take index xs) ++ reverse(take (length xs - index - 1) (reverse xs))

-- | 'deleteEntryAtIndex' deletes an entry from the table
-- First argument is the index in table for the entry to be deleted
-- Second argument is the database (Maybe Database)
-- Third argument is table name
deleteEntryAtIndex :: Int -> Maybe Database -> String -> Maybe Database
deleteEntryAtIndex index db tableName
    | isNothing db = Nothing
    | otherwise = do
        let my_db = fromJust db
        newDatabase (dName my_db) (Data.Map.adjust f tableName $ dTables my_db)
    where f = \table -> (fromJust $ newTable (tName table) (tColNameList table, (Data.Map.fromList new_cols)))
          cols = Data.Map.elems $ tColumns $ fromJust $ getTable tableName db
          new_cols = [(cName col, fromJust $ newColumn (cName col) (cDatatype col) (deleteFromList index $ cValues col)) | col <- cols]

deleteEntryAtIndices :: [Int] -> Maybe Database -> String -> Maybe Database
deleteEntryAtIndices indexList db tableName
    | isNothing db || indexList == [] || not (containsTable tableName db) = db
    | otherwise =
        let (index:indices) = indexList
            newDB = deleteEntryAtIndex index db tableName
        in deleteEntryAtIndices [index-1 | index <- indices] newDB tableName

delete :: Either Text.Parsec.Error.ParseError Exp.ValueExpr -> Maybe Database -> String -> Maybe Database
delete (Right condition) db tableName
    | isNothing db || not (containsTable tableName db) = db
    | otherwise =
        let table = getTable tableName db
            nEntries = count table
            columns = Data.Map.elems $ tColumns $ fromJust table 
            indices = [n | n <- [0..nEntries-1], let map = Data.Map.fromList [(colName, value) | col <- columns, let colName = cName col, let value = litValue ((cValues col) !! n) (cDatatype col)], Exp.evaluateExpr2 map condition]
        in deleteEntryAtIndices indices db tableName

-- | 'orderBy' orders the entries given by find based on the value given by expression
-- First argument is the expression that gives the deciding value
-- Second argument is the entrylist returned by find / select
orderBy :: Either Text.Parsec.Error.ParseError Exp.ValueExpr -> [[(String, Datatype, String)]] -> [[(String, Datatype, String)]]
orderBy (Right expr) entryList
    | length entryList == 0 = []
    | otherwise =
        sortBy (\entry1 entry2-> 
            let map1 = Data.Map.fromList [(colName, litValue (value) colDatatype) | (colName, colDatatype, value) <- entry1]
                map2 = Data.Map.fromList [(colName, litValue (value) colDatatype) | (colName, colDatatype, value) <- entry2]
                val1 = Exp.evaluateExpr map1 expr
                val2 = Exp.evaluateExpr map2 expr
            in if val1 > val2 then GT else if val1 < val2 then LT else EQ) entryList

-- | 'select' selects specific columns from the output of find/orderBy and returns list of entries with those columns only
-- First argument is column alias (if new name of the columns is needed, then this is used). If this is empty, all columns with default names are returned
-- Second argument is output of find/orderBy
select :: [(String, String)] -> [[(String, Datatype, String)]] -> [[(String, Datatype, String)]]
select colAlias entryList
    | length entryList == 0 = []
    | length colAlias == 0 = entryList
    | otherwise =
        let alias = Data.Map.fromList colAlias
        in [newEntry | entry <- entryList, 
            let newEntry = [(fromJust newName, datatype, value) | (name, datatype, value) <- entry, let newName = Data.Map.lookup name alias, not (isNothing newName)]]

