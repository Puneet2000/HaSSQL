module ParserTests where
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
import Database(Datatype(..))
import Data.Map (Map)
import qualified Data.Map as Map

parserTests :: IO Counts
parserTests = test1 *> test2 *> test3 *> test4 *> do
 let map0 = Map.empty
 let map1 = Map.insert "x" (BoolLit False) map0
 let map2 = Map.insert "y" (NumLit 1) map1
 let map3 = Map.insert "z" (NumLit 2) map2
 let map4 = Map.insert "p" (NumLit 3) map3
 let map5 = Map.insert "q" (NumLit 4) map4
 let map6 = Map.insert "r" (BoolLit True) map5
 test5 map6
 test6 map6

test1 = H.runTestTT $ H.TestList $ map (makeTest (valueExpr [])) basicTests
test2 = H.runTestTT $ H.TestList $ map (makeTest queryExpr) allQueryExprTests
test3 = H.runTestTT $ H.TestList $ map (makeTest insertExpr) insertTests
test4 = H.runTestTT $ H.TestList $ map (makeTest createExpr) createTests
test5 m = H.runTestTT $ H.TestList $ map (makeTest2 evaluate m (valueExpr [])) integerEvaluationTests
test6 m = H.runTestTT $ H.TestList $ map (makeTest2 evaluate2 m (valueExpr [])) boolEvaluationTests

numLitTests :: [(String,ValueExpr)]
numLitTests =
    [("1", NumLit 1)
    ,("54321", NumLit 54321)]

idenTests :: [(String,ValueExpr)]
idenTests =
    [("test", Iden "test")
    ,("_something3", Iden "_something3")]

operatorTests :: [(String,ValueExpr)]
operatorTests =
   map (\o -> (o ++ " a", PrefOp o (Iden "a"))) ["not", "+", "-"]
   ++ map (\o -> ("a " ++ o ++ " b", BinOp (Iden "a") o (Iden "b")))
          ["=",">","<", ">=", "<=", "!="
          ,"and", "or", "+", "-", "*", "/"]


parensTests :: [(String,ValueExpr)]
parensTests = [("(1)", Parens (NumLit 1))]

basicTests :: [(String,ValueExpr)]
basicTests = numLitTests ++ idenTests ++ operatorTests ++ parensTests ++ stringLiteralTests ++ starTests

singleSelectItemTests :: [(String,QueryExpr)]
singleSelectItemTests =
    [("select 1 from table1", makeSelect {qeSelectList = [(NumLit 1,Nothing)]
                                          ,qefromClause = Iden "table1"})]

multipleSelectItemsTests :: [(String,QueryExpr)]
multipleSelectItemsTests =
    [("select a from table1"
     ,makeSelect {qeSelectList = [(Iden "a",Nothing)]
                ,qefromClause = Iden "table1"})
    ,("select a,b from table1"
     ,makeSelect {qeSelectList = [(Iden "a",Nothing),(Iden "b",Nothing)]
                  ,qefromClause = Iden "table1"})
    ,("select 1+2,3+4 from table1"
     ,makeSelect {qeSelectList =
                     [(BinOp (NumLit 1) "+" (NumLit 2),Nothing)
                     ,(BinOp (NumLit 3) "+" (NumLit 4),Nothing)]
                     , qefromClause = Iden "table1"})
    ]

selectListTests :: [(String,QueryExpr)]
selectListTests =
    [("select a as a, b as b from table1"
     ,makeSelect {qeSelectList = [(Iden "a", Just "a")
                                 ,(Iden "b", Just "b")]
                                 , qefromClause = Iden "table1"})
    ,("select a a, b b from table1"
     ,makeSelect {qeSelectList = [(Iden "a", Just "a")
                                 ,(Iden "b", Just "b")]
                                 ,qefromClause = Iden "table1"})
    ] ++ multipleSelectItemsTests
      ++ singleSelectItemTests
whereTests :: [(String,QueryExpr)]
whereTests =
    [("select a from table1 where a = 5"
     ,makeSelect {qeSelectList = [(Iden "a",Nothing)]
                 ,qefromClause = Iden "table1"
                 ,qeWhere = Just $ BinOp (Iden "a") "=" (NumLit 5)})
    ,("select * from table1 where a = 5"
     ,makeSelect {qeSelectList = [(Star,Nothing)]
                 ,qefromClause = Iden "table1"
                 ,qeWhere = Just $ BinOp (Iden "a") "=" (NumLit 5)})
    ]

groupByTests :: [(String,QueryExpr)]
groupByTests =
    [("select a,b from table1 group by a"
     ,makeSelect {qeSelectList = [(Iden "a",Nothing)
                                 ,(Iden "b",Nothing)]
                 ,qefromClause = Iden "table1"
                 ,qeGroupBy = [Iden "a"]
                 })
    ]

havingTests :: [(String,QueryExpr)]
havingTests =
  [("select a,b from table1 group by a having b > 5"
     ,makeSelect {qeSelectList = [(Iden "a",Nothing)
                                 ,(Iden "b",Nothing)]
                 ,qefromClause = Iden "table1"
                 ,qeGroupBy = [Iden "a"]
                 ,qeHaving = Just $ BinOp (Iden "b") ">" (NumLit 5)
                 })

  ]

orderByTests :: [(String,QueryExpr)]
orderByTests =
    [("select a from table1 order by a"
     ,ms [Iden "a"])
    ,("select a from table1 order by a, b"
     ,ms [Iden "a", Iden "b"])
    ]
  where
    ms o = makeSelect {qeSelectList = [(Iden "a",Nothing)]
                      ,qefromClause = Iden "table1"
                      ,qeOrderBy = o}

allQueryExprTests :: [(String,QueryExpr)]
allQueryExprTests = concat [selectListTests ++ whereTests ++ groupByTests ++ havingTests ++ orderByTests]

makeTest2 :: ((Map String ValueExpr) -> Either ParseError ValueExpr -> EvalType) -> (Map String ValueExpr) -> Parser ValueExpr -> (String,EvalType) -> H.Test
makeTest2 eval m parser (src,expected) = H.TestLabel src $ H.TestCase $ do
    let gote = eval m (parse (whitespace *> parser <* eof) "" src)
    H.assertEqual src expected gote

makeTest :: (Eq a, Show a) => Parser a -> (String,a) -> H.Test
makeTest parser (src,expected) = H.TestLabel src $ H.TestCase $ do
    let gote = parse (whitespace *> parser <* eof) "" src
    case gote of
      Left e -> H.assertFailure $ show e
      Right got -> H.assertEqual src expected got

stringLiteralTests :: [(String, ValueExpr)]
stringLiteralTests =
    [("''", StringLit ""),
    ("'test'", StringLit "test")]

starTests :: [(String, ValueExpr)]
starTests = [("*", Star)]

insertTests :: [(String, InsertExpr)]
insertTests = [("insert into table1 (c1,c2) values (1,'Hello')"
                , makeInsert {iTable = Iden "table1", iColumns = [Iden "c1",Iden "c2"], iValues = [NumLit 1,StringLit "Hello"]}),
                ("insert into table1 values (1,'Hello')"
                , makeInsert {iTable = Iden "table1", iColumns = [], iValues = [NumLit 1,StringLit "Hello"]})
              ]

createTests :: [(String , CreateExpr)]
createTests = [("create table table1 ( c1 INTEGER , c2 STRING , c3 BOOL )"
                , makeCreate {iTname = Iden "table1", iColLists = [(Iden "c1",INT),(Iden "c2",STRING),(Iden "c3",BOOL)]}),
                ("create table table1 ( c1 INTEGER , c2 INTEGER )"
                , makeCreate {iTname = Iden "table1", iColLists = [(Iden "c1",INT),(Iden "c2",INT)]})
              ]



integerEvaluationTests :: [(String , EvalType)]
integerEvaluationTests = [("y+z+p",Int 6),("y*(z+p)",Int 5),("(y+z)*p",Int 9),("(q/z)+(q%3)",Int 3)]

boolEvaluationTests :: [(String , EvalType)]
boolEvaluationTests = [("x or r",Boolean True),("(x and not r) or (not x and r)",Boolean True),("r and (q>p)",Boolean True)]


