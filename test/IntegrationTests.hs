module IntegrationTests where
import Test.HUnit as H
import Text.Parsec.String (Parser)
import Text.Parsec.Error (ParseError)
import Text.Parsec (parse )
import Text.ParserCombinators.Parsec.Combinator (eof)
import Control.Applicative ((<*),(<$>), (*>), (<|>),(<$),(<*>))
import Funcs
import ExpressionParser
import QueryParser
import InsertParser
import CreateParser
import Database
import Data.Map (Map)
import qualified Data.Map as Map

integrationTests :: IO Counts
integrationTests = do
 let mdb = newDatabase "mdb1" Map.empty
 test1 mdb
 let mdb2 = evaluateCreate (regularParse createExpr "create table table1 ( c1 INTEGER , c2 STRING , c3 BOOL)") mdb
 test2 mdb2

test1 m = H.runTestTT $ H.TestList $ map (makeTest createExpr evaluateCreate m) integrationCreateTests
test2 m = H.runTestTT $ H.TestList $ map (makeTest insertExpr evaluateInsert m) integrationInsertTests

makeTest :: Parser a -> (Either ParseError a -> Maybe Database -> Maybe Database) -> Maybe Database -> (String , Maybe Database) -> H.Test
makeTest parseExpr evalExpr db (src,expected) = H.TestLabel src $ H.TestCase $ do
    let gote = evalExpr (regularParse parseExpr src) db
    H.assertEqual src expected gote


integrationCreateTests :: [(String , Maybe Database)]
integrationCreateTests = [("create table table1 ( c1 INTEGER , c2 STRING , c3 BOOL)"
                     ,Just (Database {dName = "mdb1"
                    , dTables = Map.fromList [("table1",Table {tName = "table1"
                    , tColumns = Map.fromList [("c1",Column {cName = "c1", cDatatype = INT, cValues = []})
                    ,("c2",Column {cName = "c2", cDatatype = STRING, cValues = []})
                    ,("c3",Column {cName = "c3", cDatatype = BOOL, cValues = []})]})]}))
                    
                    ,("create table table1 ( c1 INTEGER , c2 INTEGER )"
                     , Just (Database {dName = "mdb1"
                    , dTables = Map.fromList [("table1",Table {tName = "table1"
                    , tColumns = Map.fromList [("c1",Column {cName = "c1", cDatatype = INT, cValues = []})
                    ,("c2",Column {cName = "c2", cDatatype = INT, cValues = []})]})]}))]

integrationInsertTests :: [(String , Maybe Database)]
integrationInsertTests = [("insert into table1 (c1,c2,c3) values (1,'Hello',True)"
                         ,Just (Database {dName = "mdb1"
                         , dTables = Map.fromList [("table1",Table {tName = "table1"
                         , tColumns = Map.fromList [("c1",Column {cName = "c1", cDatatype = INT, cValues = ["1"]})
                         ,("c2",Column {cName = "c2", cDatatype = STRING, cValues = ["Hello"]})
                         ,("c3",Column {cName = "c3", cDatatype = BOOL, cValues = ["True"]})]})]}))
                    
                         ,("insert into table1 (c1,c2,c3) values (2,'Puneet',False)"
                         , Just (Database {dName = "mdb1"
                         , dTables = Map.fromList [("table1",Table {tName = "table1"
                         , tColumns = Map.fromList [("c1",Column {cName = "c1"
                         , cDatatype = INT, cValues = ["2"]})
                         ,("c2",Column {cName = "c2", cDatatype = STRING, cValues = ["Puneet"]})
                         ,("c3",Column {cName = "c3", cDatatype = BOOL, cValues = ["False"]})]})]}))]


