module Main where
import Test.HUnit as H
import Text.Parsec.String (Parser)
import Text.Parsec.Error (ParseError)
import Text.Parsec (parse )
import Text.ParserCombinators.Parsec.Combinator (eof)
import Control.Applicative ((<*),(<$>), (*>), (<|>),(<$),(<*>))
import Funcs
import ExpressionParser
import QueryParser

main :: IO Counts
main = test1 *> test2
test1 = H.runTestTT $ H.TestList $ map (makeTest (valueExpr [])) basicTests
test2 = H.runTestTT $ H.TestList $ map (makeTest queryExpr) allQueryExprTests
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
          ["=",">","<", ">=", "<=", "!=", "<>"
          ,"and", "or", "+", "-", "*", "/", "||", "like"]


parensTests :: [(String,ValueExpr)]
parensTests = [("(1)", Parens (NumLit 1))]

basicTests :: [(String,ValueExpr)]
basicTests = numLitTests ++ idenTests ++ operatorTests ++ parensTests 

singleSelectItemTests :: [(String,QueryExpr)]
singleSelectItemTests =
    [("select 1", makeSelect {qeSelectList = [(NumLit 1,Nothing)]})]

multipleSelectItemsTests :: [(String,QueryExpr)]
multipleSelectItemsTests =
    [("select a"
     ,makeSelect {qeSelectList = [(Iden "a",Nothing)]})
    ,("select a,b"
     ,makeSelect {qeSelectList = [(Iden "a",Nothing)
                                 ,(Iden "b",Nothing)]})
    ,("select 1+2,3+4"
     ,makeSelect {qeSelectList =
                     [(BinOp (NumLit 1) "+" (NumLit 2),Nothing)
                     ,(BinOp (NumLit 3) "+" (NumLit 4),Nothing)]})
    ]

selectListTests :: [(String,QueryExpr)]
selectListTests =
    [("select a as a, b as b"
     ,makeSelect {qeSelectList = [(Iden "a", Just "a")
                                 ,(Iden "b", Just "b")]})
    ,("select a a, b b"
     ,makeSelect {qeSelectList = [(Iden "a", Just "a")
                                 ,(Iden "b", Just "b")]})
    ] ++ multipleSelectItemsTests
      ++ singleSelectItemTests

whereTests :: [(String,QueryExpr)]
whereTests =
    [("select a where a = 5"
     ,makeSelect {qeSelectList = [(Iden "a",Nothing)]
                 ,qeWhere = Just $ BinOp (Iden "a") "=" (NumLit 5)})
    ]

groupByTests :: [(String,QueryExpr)]
groupByTests =
    [("select a,b group by a"
     ,makeSelect {qeSelectList = [(Iden "a",Nothing)
                                 ,(Iden "b",Nothing)]
                 ,qeGroupBy = [Iden "a"]
                 })
    ]

havingTests :: [(String,QueryExpr)]
havingTests =
  [("select a,b group by a having b > 5"
     ,makeSelect {qeSelectList = [(Iden "a",Nothing)
                                 ,(Iden "b",Nothing)]
                 ,qeGroupBy = [Iden "a"]
                 ,qeHaving = Just $ BinOp (Iden "b") ">" (NumLit 5)
                 })

  ]

orderByTests :: [(String,QueryExpr)]
orderByTests =
    [("select a order by a"
     ,ms [Iden "a"])
    ,("select a order by a, b"
     ,ms [Iden "a", Iden "b"])
    ]
  where
    ms o = makeSelect {qeSelectList = [(Iden "a",Nothing)]
                      ,qeOrderBy = o}

allQueryExprTests :: [(String,QueryExpr)]
allQueryExprTests = concat [selectListTests ++ whereTests ++ groupByTests ++ havingTests ++ orderByTests]

makeTest :: (Eq a, Show a) => Parser a -> (String,a) -> H.Test
makeTest parser (src,expected) = H.TestLabel src $ H.TestCase $ do
    let gote = parse (whitespace *> parser <* eof) "" src
    case gote of
      Left e -> H.assertFailure $ show e
      Right got -> H.assertEqual src expected got
