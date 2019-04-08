module ExpressionParser where

import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Char 
import Text.ParserCombinators.Parsec.Combinator 
import Text.Parsec (parse , ParseError , try)
import Control.Applicative (many, (<*),(<$>), (*>), (<|>),(<$),(<*>))
import Control.Monad (void,guard)
import qualified Text.ParserCombinators.Parsec.Expr as E
import qualified Control.Exception as Ex
import System.IO

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
term blacklist = boolean <|> iden blacklist  <|> num <|> parensValue <|> stringLit <|> star

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

evaluate :: Either ParseError ValueExpr -> EvalType
evaluate (Right e) = Int (evaluateExpr e)
evaluate (Left e) = Error (show e)

evaluate2 :: Either ParseError ValueExpr -> EvalType
evaluate2 (Right e) = Boolean (evaluateExpr2 e)
evaluate2 (Left e) = Error (show e)

evaluateExpr :: ValueExpr -> Integer
evaluateExpr (NumLit n) = n
evaluateExpr (Parens v) = evaluateExpr v
evaluateExpr (PrefOp "-" v) = -(evaluateExpr v)
evaluateExpr (PrefOp "+" v) = (evaluateExpr v)
evaluateExpr (BinOp v1 "^" v2) = (evaluateExpr v1)^(evaluateExpr v2)
evaluateExpr (BinOp v1 "*" v2) = (evaluateExpr v1)*(evaluateExpr v2)
evaluateExpr (BinOp v1 "/" v2) = div (evaluateExpr v1) (evaluateExpr v2)
evaluateExpr (BinOp v1 "%" v2) = mod (evaluateExpr v1) (evaluateExpr v2)
evaluateExpr (BinOp v1 "+" v2) = (evaluateExpr v1) + (evaluateExpr v2)

evaluateExpr2 :: ValueExpr -> Bool
evaluateExpr2 (BoolLit True) = True
evaluateExpr2 (BoolLit False) = False
evaluateExpr2 (Parens v) = evaluateExpr2 v
evaluateExpr2 (BinOp v1 "<=" v2) = (evaluateExpr v1) <= (evaluateExpr v2)
evaluateExpr2 (BinOp v1 ">=" v2) = (evaluateExpr v1) >= (evaluateExpr v2)
evaluateExpr2 (BinOp v1 "!=" v2) = (evaluateExpr v1) /= (evaluateExpr v2)
evaluateExpr2 (BinOp v1 ">" v2) = (evaluateExpr v1) > (evaluateExpr v2)
evaluateExpr2 (BinOp v1 "<" v2) = (evaluateExpr v1) < (evaluateExpr v2)
evaluateExpr2 (BinOp v1 "=" v2) = (evaluateExpr v1) == (evaluateExpr v2)
evaluateExpr2 (PrefOp "not" v) = not (evaluateExpr2 v)
evaluateExpr2 (BinOp v1 "and" v2) = (&&) (evaluateExpr2 v1) (evaluateExpr2 v2)
evaluateExpr2 (BinOp v1 "or" v2) = (||) (evaluateExpr2 v1) (evaluateExpr2 v2)

