module Main where
import Test.HUnit as H
import Text.Parsec.String (Parser)
import Text.Parsec.Error (ParseError)
import Text.Parsec (parse )
import Text.ParserCombinators.Parsec.Combinator (eof)
import Funcs
import BasicParsers

main :: IO Counts
main = H.runTestTT $ H.TestList $ map (makeTest valueExpr) basicTests

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

caseTests :: [(String,ValueExpr)]
caseTests =
    [("case a when 1 then 2 end"
     ,Case (Just $ Iden "a") [(NumLit 1,NumLit 2)] Nothing)

    ,("case a when 1 then 2 when 3 then 4 end"
     ,Case (Just $ Iden "a")
           [(NumLit 1, NumLit 2)
           ,(NumLit 3, NumLit 4)]
           Nothing)

    ,("case a when 1 then 2 when 3 then 4 else 5 end"
     ,Case (Just $ Iden "a")
           [(NumLit 1, NumLit 2)
           ,(NumLit 3, NumLit 4)]
           (Just $ NumLit 5))

    ,("case when a=1 then 2 when a=3 then 4 else 5 end"
     ,Case Nothing
           [(BinOp (Iden "a") "=" (NumLit 1), NumLit 2)
           ,(BinOp (Iden "a") "=" (NumLit 3), NumLit 4)]
           (Just $ NumLit 5))
    ]

basicTests :: [(String,ValueExpr)]
basicTests = numLitTests ++ idenTests ++ operatorTests ++ parensTests ++ caseTests

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