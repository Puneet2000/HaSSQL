module Database where

import qualified Data.Map
import Data.Maybe (fromJust, isNothing)
import Data.List (elemIndex, elemIndices)

-- for debugging only. To be removed once completed - called from main right now!
sampleCommands = do
    print "======================================="
    
    let my_db = newDatabase "my_db 1" Data.Map.empty
    -- print $ my_db

    let my_db_1 = addTable "table 1" my_db
    -- print $ my_db_1

    let my_db = my_db_1
    let my_db_1 = addTable "table 2" my_db
    -- print $ my_db_1

    let my_db = my_db_1
    let contains = containsTable "table 1" my_db
    -- print $ contains

    let my_db_1 = addColumn "Name" STRING my_db "table 1"
    let my_db = my_db_1
    let my_db_1 = addColumn "Phone number" INT my_db "table 1"
    -- print my_db_1

    -- let my_db = my_db_1
    -- let my_db_1 = insertOne "23" INT my_db "table 1" "column 1 for table 1"
    -- print my_db_1
    
    -- let my_db = my_db_1
    -- let my_db_1 = insertOne "45" INT my_db "table 1" "column 1 for table 1"
    -- print my_db_1

    let my_db = my_db_1
    let my_db_1 = insert ["Name", "Phone number"] ["Martha", "12345"] [STRING, INT] my_db "table 1"
    -- print my_db_1

    let my_db = my_db_1
    let my_db_1 = insert ["Name", "Phone number"] ["Stewart", "78910"] [STRING, INT] my_db "table 1"
    -- print my_db_1


    let my_db = my_db_1
    let my_db_1 = insert ["Name", "Phone number"] ["Stewart", "12321"] [STRING, INT] my_db "table 1"

    let x = find "Stewart" my_db_1 "table 1" "Name"
    print(my_db_1)
    let my_db = deleteEntryAtIndex 3 my_db_1 "table 1"
    print(my_db)
    
    -- print my_db_1

    return 0

data Database = Database {
    dName :: String,
    dTables :: Data.Map.Map String Table
} deriving (Show)

-- Returns new database with name and tables
newDatabase :: String -> Data.Map.Map String Table -> Maybe Database
newDatabase name tables = Just Database {
    dName = name,
    dTables = tables
}

data Table = Table {
    tName :: String,
    tColumns :: Data.Map.Map String Column
} deriving (Show)

-- Returns new table with name and columns
newTable :: String -> Data.Map.Map String Column -> Maybe Table
newTable name columns = Just Table {
    tName = name,
    tColumns = columns
}

-- For datatype in a column
data Datatype = INT | STRING | BOOL deriving (Show, Eq)

data Column = Column {
    cName :: String,
    cDatatype :: Datatype, -- For Num => Num, String => String, Bool => Bool
    cValues :: [String]
} deriving (Show)

-- Returns new column with name, datatype and values
newColumn :: String -> Datatype -> [String] -> Maybe Column
newColumn name datatype values = Just Column {
    cName = name,
    cDatatype = datatype,
    cValues = values
}

-- Adds a table to database with tableName
addTable :: String -> Maybe Database -> Maybe Database
addTable tableName database
    | isNothing database = Nothing
    | otherwise = Just Database {
        dName = dName $ fromJust database,
        dTables = Data.Map.insert tableName (fromJust $ newTable tableName Data.Map.empty) (dTables $ fromJust database)
    }

-- Returns table from database with name tableName
getTable :: String -> Maybe Database -> Maybe Table
getTable tableName database
    | isNothing database = Nothing
    | otherwise = Data.Map.lookup tableName $ dTables $ fromJust database

-- True if database contains table named tableName
containsTable :: String -> Maybe Database -> Bool
containsTable tableName database = not (isNothing $ getTable tableName database)

-- Adds column to table object with name and datatype
addColumnToTable :: String -> Datatype -> Table -> Table
addColumnToTable columnName datatype table = fromJust $ newTable (tName table)
        (Data.Map.insert columnName (fromJust $ newColumn columnName datatype []) (tColumns table))

-- Adds a column to database -> tableName with name=columnName, datatype=datatype
addColumn :: String -> Datatype -> Maybe Database -> String -> Maybe Database
addColumn columnName datatype database tableName
    | isNothing database || not (containsTable tableName database) = Nothing
    | otherwise = newDatabase (dName $ fromJust database)
            (Data.Map.adjust (addColumnToTable columnName datatype) tableName $ dTables $ fromJust database)

-- Returns column with columnName in database -> tableName
getColumn :: String -> Maybe Database -> String -> Maybe Column
getColumn columnName database tableName
    | isNothing database || not (containsTable tableName database) = Nothing
    | otherwise = Data.Map.lookup columnName (tColumns $ fromJust $ getTable tableName database)

-- True if database -> tableName contains a column named columnName
containsColumn :: String -> Maybe Database -> String -> Bool
containsColumn columnName database tableName = not (isNothing $ getColumn columnName database tableName)

-- Input value and datatype to be inserted in a single column
insertOne :: String -> Datatype -> Maybe Database -> String -> String -> Maybe Database
insertOne value datatype database tableName columnName
    | isNothing database || not (containsTable tableName database) || not (containsColumn columnName database tableName)
        = database -- Invalid table name or col name
    | datatype /= cDatatype (fromJust $ getColumn columnName database tableName )= database -- Datatypes dont match
    | otherwise = newDatabase (dName $ fromJust database) (Data.Map.adjust f tableName $ dTables $ fromJust database)
    where f = (\table -> fromJust $ newTable (tName table) (Data.Map.adjust g columnName $ tColumns table))
          g = (\column -> fromJust $ newColumn (cName column) (cDatatype column) ((cValues column) ++ [value]))

-- Input - list of colNames <-> corresponding list of values <-> corresponding list of datatypes
insert :: [String] -> [String] -> [Datatype] -> Maybe Database -> String -> Maybe Database
insert columnNames values datatypes db tableName 
    | length values /= length datatypes || length columnNames /= length values = db --Mismatch in length of names, values and datatypes
    | length values == 0 = db -- Base case
    | otherwise = do
        let v = head values; vs = tail values
        let d = head datatypes; ds = tail datatypes
        let col = head columnNames; cols = tail columnNames
        let new_db = insertOne v d db tableName col
        insert cols vs ds new_db tableName

-- Utility functions for find*
getColList table db= Data.Map.elems (tColumns $ fromJust $ getTable table db)

-- Finds entry at index in table and returns as [tuples] where tuple = (col name, col type, col value)
findEntryAtIndex :: String -> Maybe Database -> Int -> [(String, Datatype, String)]
findEntryAtIndex tableName db index = [(cName col, cDatatype col, (cValues col) !! index) | col <- getColList tableName db]

-- Finds all entries in (db -> table) with value, datype and returns as [entries] where entry=[tuples] where tuple = (col name, col type, col value)
find :: String -> Maybe Database -> String -> String -> [[(String, Datatype, String)]]
find value db tableName columnName
    | isNothing db || not (containsColumn columnName db tableName) = []
    | otherwise = do
        let col = getColumn columnName db tableName
        let indices = elemIndices value (cValues $ fromJust col)
        if length indices == 0 then [] -- no such index exists
        else [list | index <- indices, let list = findEntryAtIndex tableName db index]

deleteFromList :: Int -> [String] -> [String]
deleteFromList index xs = (take index xs) ++ reverse(take (length xs - index - 1) (reverse xs))

-- Delete an entry from the table
deleteEntryAtIndex :: Int -> Maybe Database -> String -> Maybe Database
deleteEntryAtIndex index db tableName
    | isNothing db = Nothing
    | otherwise = do
        let my_db = fromJust db
        newDatabase (dName my_db) (Data.Map.adjust f tableName $ dTables my_db)
    where f = \table -> (fromJust $ newTable (tName table) (Data.Map.fromList new_cols))
          cols = Data.Map.elems $ tColumns $ fromJust $ getTable tableName db
          new_cols = [(cName col, fromJust $ newColumn (cName col) (cDatatype col) (deleteFromList index $ cValues col)) | col <- cols]