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
 let mdb3 = evaluateInsert (regularParse insertExpr "insert into table1 values (1,'Puneet',True)") mdb2
 let mdb4 = evaluateInsert (regularParse insertExpr "insert into table1 values (11,'Shraiysh',False)") mdb3
 let mdb5 = evaluateInsert (regularParse insertExpr "insert into table1 values (1,'Sai ramana',True)") mdb4
 let mdb6 = evaluateInsert (regularParse insertExpr "insert into table1 values (2,'Hitesh',False)") mdb5
 let mdb7 = evaluateInsert (regularParse insertExpr "insert into table1 values (5,'POPL',True)") mdb6
 let mdb8 = evaluateInsert (regularParse insertExpr "insert into table1 values (7,'Project',True)") mdb7
 test3 mdb8

test1 m = H.runTestTT $ H.TestList $ map (makeTest createExpr evaluateCreate m) integrationCreateTests
test2 m = H.runTestTT $ H.TestList $ map (makeTest insertExpr evaluateInsert m) integrationInsertTests
test3 m = H.runTestTT $ H.TestList $ map (makeTest2 queryExpr evaluateQuery m) integrationQueryTests

makeTest :: Parser a -> (Either ParseError a -> Maybe Database -> Maybe Database) -> Maybe Database -> (String , Maybe Database) -> H.Test
makeTest parseExpr evalExpr db (src,expected) = H.TestLabel src $ H.TestCase $ do
    let gote = evalExpr (regularParse parseExpr src) db
    H.assertEqual src expected gote

makeTest2 :: Parser a -> (Either ParseError a -> Maybe Database -> [[(Int,String, Datatype, String)]]) -> Maybe Database -> (String , [[(Int,String, Datatype, String)]]) -> H.Test
makeTest2 parseExpr evalExpr db (src,expected) = H.TestLabel src $ H.TestCase $ do
    let gote = evalExpr (regularParse parseExpr src) db
    H.assertEqual src expected gote


integrationCreateTests :: [(String , Maybe Database)]
integrationCreateTests = [("create table table1 ( c1 INTEGER , c2 STRING , c3 BOOL)"
                     ,Just (Database {dName = "mdb1"
                    , dTables = Map.fromList [("table1",Table {tName = "table1"
                    ,tColNameList = ["c1","c2","c3"]
                    , tColumns = Map.fromList [("c1",Column {cName = "c1", cDatatype = INT, cValues = []})
                    ,("c2",Column {cName = "c2", cDatatype = STRING, cValues = []})
                    ,("c3",Column {cName = "c3", cDatatype = BOOL, cValues = []})]
                    , tPKeys = []})]}))
                    
                    ,("create table table1 ( c1 INTEGER , c2 INTEGER )"
                     , Just (Database {dName = "mdb1"
                    , dTables = Map.fromList [("table1",Table {tName = "table1"
                    ,tColNameList = ["c1","c2"]
                    , tColumns = Map.fromList [("c1",Column {cName = "c1", cDatatype = INT, cValues = []})
                    ,("c2",Column {cName = "c2", cDatatype = INT, cValues = []})]
                    , tPKeys = []})]}))]

integrationInsertTests :: [(String , Maybe Database)]
integrationInsertTests = [("insert into table1 (c1,c2,c3) values (1,'Hello',True)"
                         ,Just (Database {dName = "mdb1"
                         , dTables = Map.fromList [("table1",Table {tName = "table1"
                        ,tColNameList = ["c1","c2","c3"]
                         , tColumns = Map.fromList [("c1",Column {cName = "c1", cDatatype = INT, cValues = ["1"]})
                         ,("c2",Column {cName = "c2", cDatatype = STRING, cValues = ["Hello"]})
                         ,("c3",Column {cName = "c3", cDatatype = BOOL, cValues = ["True"]})]
                         , tPKeys = [0]})]}))
                    
                         ,("insert into table1 (c1,c2,c3) values (2,'Puneet',False)"
                         , Just (Database {dName = "mdb1"
                         , dTables = Map.fromList [("table1",Table {tName = "table1"
                         ,tColNameList = ["c1","c2","c3"]
                         , tColumns = Map.fromList [("c1",Column {cName = "c1"
                         , cDatatype = INT, cValues = ["2"]})
                         ,("c2",Column {cName = "c2", cDatatype = STRING, cValues = ["Puneet"]})
                         ,("c3",Column {cName = "c3", cDatatype = BOOL, cValues = ["False"]})]
                         , tPKeys = [0]})]}))

                         ,("insert into table1 values (1,'Hello',True)"
                         ,Just (Database {dName = "mdb1"
                         , dTables = Map.fromList [("table1",Table {tName = "table1"
                         ,tColNameList = ["c1","c2","c3"]
                         , tColumns = Map.fromList [("c1",Column {cName = "c1", cDatatype = INT, cValues = ["1"]})
                         ,("c2",Column {cName = "c2", cDatatype = STRING, cValues = ["Hello"]})
                         ,("c3",Column {cName = "c3", cDatatype = BOOL, cValues = ["True"]})]
                         , tPKeys = [0]})]}))
                         ]

integrationQueryTests :: [(String , [[(Int,String, Datatype, String)]])]
integrationQueryTests = [("select c1,c2 from table1 order by -c1",
                           [[(1, "c1",INT,"11"),(1, "c2",STRING,"Shraiysh")]
                           ,[(5, "c1",INT,"7"),(5, "c2",STRING,"Project")]
                           ,[(4, "c1",INT,"5"),(4, "c2",STRING,"POPL")]
                           ,[(3, "c1",INT,"2"),(3, "c2",STRING,"Hitesh")]
                           ,[(0, "c1",INT,"1"),(0, "c2",STRING,"Puneet")]
                           ,[(2, "c1",INT,"1"),(2, "c2",STRING,"Sai ramana")]]
                          )
                         , ("select c1,c2,c3 from table1 order by c1*c1 - c1",
                            [[(0, "c1",INT,"1"),(0, "c2",STRING,"Puneet"),(0, "c3",BOOL,"True")]
                            ,[(2, "c1",INT,"1"),(2, "c2",STRING,"Sai ramana"),(2, "c3",BOOL,"True")]
                            ,[(3, "c1",INT,"2"),(3, "c2",STRING,"Hitesh"),(3, "c3",BOOL,"False")]
                            ,[(4, "c1",INT,"5"),(4, "c2",STRING,"POPL"),(4, "c3",BOOL,"True")]
                            ,[(5, "c1",INT,"7"),(5, "c2",STRING,"Project"),(5, "c3",BOOL,"True")]
                            ,[(1, "c1",INT,"11"),(1, "c2",STRING,"Shraiysh"),(1, "c3",BOOL,"False")]]
                            )
                         , ("select c1,c2 from table1 where c1<5 order by -c1",
                            [[(3, "c1",INT,"2"),(3, "c2",STRING,"Hitesh")]
                            ,[(0, "c1",INT,"1"),(0, "c2",STRING,"Puneet")]
                            ,[(2, "c1",INT,"1"),(2, "c2",STRING,"Sai ramana")]]
                            )
                         , ("select c1,c2 from table1 where c3 order by -c1",
                            [[(5, "c1",INT,"7"),(5, "c2",STRING,"Project")]
                            ,[(4, "c1",INT,"5"),(4, "c2",STRING,"POPL")]
                            ,[(0, "c1",INT,"1"),(0, "c2",STRING,"Puneet")]
                            ,[(2, "c1",INT,"1"),(2, "c2",STRING,"Sai ramana")]]
                            )
                         ,("select * from table1 where c3 order by c1",
                           [[(0, "c1",INT,"1"),(0, "c2",STRING,"Puneet"),(0, "c3",BOOL,"True")]
                           ,[(2, "c1",INT,"1"),(2, "c2",STRING,"Sai ramana"),(2, "c3",BOOL,"True")]
                           ,[(4, "c1",INT,"5"),(4, "c2",STRING,"POPL"),(4, "c3",BOOL,"True")]
                           ,[(5, "c1",INT,"7"),(5, "c2",STRING,"Project"),(5, "c3",BOOL,"True")]]
                          )
                         ,("select * from table1",
                           [[(0, "c1",INT,"1"),(0, "c2",STRING,"Puneet"),(0, "c3",BOOL,"True")]
                           ,[(1, "c1",INT,"11"),(1, "c2",STRING,"Shraiysh"),(1, "c3",BOOL,"False")]
                           ,[(2, "c1",INT,"1"),(2, "c2",STRING,"Sai ramana"),(2, "c3",BOOL,"True")]
                           ,[(3, "c1",INT,"2"),(3, "c2",STRING,"Hitesh"),(3, "c3",BOOL,"False")]
                           ,[(4, "c1",INT,"5"),(4, "c2",STRING,"POPL"),(4, "c3",BOOL,"True")]
                           ,[(5, "c1",INT,"7"),(5, "c2",STRING,"Project"),(5, "c3",BOOL,"True")]]
                          )
                         ]


