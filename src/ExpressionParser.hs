module ExpressionParser where

import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Char 
import Text.ParserCombinators.Parsec.Combinator 
import Text.Parsec (parse , ParseError , try)
import Control.Applicative (many, (<*),(<$>), (*>), (<|>),(<$),(<*>))
import Control.Monad (void,guard)
import qualified Text.ParserCombinators.Parsec.Expr as E
import Data.Map (Map)
import qualified Data.Map as Map
import Funcs

data ValueExpr =  NumLit Integer
               | Iden String
               | PrefOp String ValueExpr
               | BinOp ValueExpr String ValueExpr
               | Parens ValueExpr
               | StringLit String
               | Star 
               | BoolLit Bool
                 deriving (Eq,Show)

data EvalType = Int Integer | Boolean Bool | Error String deriving (Eq,Show)
num :: Parser ValueExpr
num = NumLit <$> integer

iden :: [String] -> Parser ValueExpr
iden blacklist = Iden <$> identifierBlacklist (blacklist++["True","False","not","and","or"])

boolean :: Parser ValueExpr
boolean =  BoolLit <$> boolToken

parensValue :: Parser ValueExpr
parensValue = Parens <$> parens (valueExpr [])

term :: [String] -> Parser ValueExpr
term blacklist = try boolean <|> iden blacklist  <|> num <|> parensValue <|> stringLit <|> star

--table :: [[E.Operator ValueExpr]]
table = [[prefix "-", prefix "+"]
         ,[binary "^" E.AssocLeft]
         ,[binary "*" E.AssocLeft
          ,binary "/" E.AssocLeft
          ,binary "%" E.AssocLeft]
         ,[binary "+" E.AssocLeft
          ,binary "-" E.AssocLeft]
         ,[binary "<=" E.AssocRight
          ,binary ">=" E.AssocRight
          ,binary "!=" E.AssocRight]
         ,[binary "<" E.AssocNone
          ,binary ">" E.AssocNone]
         ,[binary "=" E.AssocRight]
         ,[prefixK "not"]
         ,[binaryK "and" E.AssocLeft]
         ,[binaryK "or" E.AssocLeft]]
  where
    binary name assoc =
        E.Infix (mkBinOp name <$ symbol name) assoc
    mkBinOp nm a b = BinOp a nm b
    prefix name = E.Prefix (PrefOp name <$ symbol name)
    binaryK name assoc =
        E.Infix (mkBinOp name <$ keyword name) assoc
    prefixK name = E.Prefix (PrefOp name <$ keyword name)

valueExpr :: [String] -> Parser ValueExpr
valueExpr blacklist = E.buildExpressionParser table (term blacklist)

stringLit :: Parser ValueExpr
stringLit = StringLit <$> stringToken

term1 :: Parser ValueExpr
term1 = iden [] <|> num <|> parensValue <|> stringLit

valueExpr1 :: Parser ValueExpr
valueExpr1 = E.buildExpressionParser table term1

star :: Parser ValueExpr
star = Star <$ symbol "*"

evaluate :: (Map String ValueExpr) -> Either ParseError ValueExpr -> EvalType
evaluate map (Right e) = Int (evaluateExpr map e)
evaluate map (Left e) = Error (show e)

evaluate2 :: (Map String ValueExpr) -> Either ParseError ValueExpr -> EvalType
evaluate2 map (Right e) = Boolean (evaluateExpr2 map e)
evaluate2 map (Left e) = Error (show e)

evaluateExpr :: (Map String ValueExpr) -> ValueExpr -> Integer
evaluateExpr map (Iden s) = do
 let val = Map.lookup s map
 case val of
  Just (NumLit i) -> i
evaluateExpr map (NumLit n) = n
evaluateExpr map (Parens v) = evaluateExpr map v
evaluateExpr map (PrefOp "-" v) = -(evaluateExpr map v)
evaluateExpr map (PrefOp "+" v) = (evaluateExpr map v)
evaluateExpr map (BinOp v1 "^" v2) = (evaluateExpr map v1)^(evaluateExpr map v2)
evaluateExpr map (BinOp v1 "*" v2) = (evaluateExpr map v1)*(evaluateExpr map v2)
evaluateExpr map (BinOp v1 "/" v2) = div (evaluateExpr map v1) (evaluateExpr map v2)
evaluateExpr map (BinOp v1 "%" v2) = mod (evaluateExpr map v1) (evaluateExpr map v2)
evaluateExpr map (BinOp v1 "+" v2) = (evaluateExpr map v1) + (evaluateExpr map v2)

evaluateExpr2 :: (Map String ValueExpr) -> ValueExpr -> Bool
evaluateExpr2 map (Iden s) = do
 let val = Map.lookup s map
 case val of
  Just (BoolLit i) -> i
evaluateExpr2 map (BoolLit True) = True
evaluateExpr2 map (BoolLit False) = False
evaluateExpr2 map (Parens v) = evaluateExpr2 map v
evaluateExpr2 map (BinOp v1 "<=" v2) = (evaluateExpr map v1) <= (evaluateExpr map v2)
evaluateExpr2 map (BinOp v1 ">=" v2) = (evaluateExpr map v1) >= (evaluateExpr map v2)
evaluateExpr2 map (BinOp v1 "!=" v2) = (evaluateExpr map v1) /= (evaluateExpr map v2)
evaluateExpr2 map (BinOp v1 ">" v2) = (evaluateExpr map v1) > (evaluateExpr map v2)
evaluateExpr2 map (BinOp v1 "<" v2) = (evaluateExpr map v1) < (evaluateExpr map v2)
evaluateExpr2 map (BinOp v1 "=" v2) = (evaluateExpr map v1) == (evaluateExpr map v2)
evaluateExpr2 map (PrefOp "not" v) = not (evaluateExpr2 map v)
evaluateExpr2 map (BinOp v1 "and" v2) = (&&) (evaluateExpr2 map v1) (evaluateExpr2 map v2)
evaluateExpr2 map (BinOp v1 "or" v2) = (||) (evaluateExpr2 map v1) (evaluateExpr2 map v2)

