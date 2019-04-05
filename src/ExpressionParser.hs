module ExpressionParser where

import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Char 
import Text.ParserCombinators.Parsec.Combinator 
import Text.Parsec (parse , ParseError , try)
import Control.Applicative (many, (<*),(<$>), (*>), (<|>),(<$),(<*>))
import Control.Monad (void,guard)
import qualified Text.ParserCombinators.Parsec.Expr as E

import Funcs

data ValueExpr =  NumLit Integer
               | Iden String
               | PrefOp String ValueExpr
               | BinOp ValueExpr String ValueExpr
               | Parens ValueExpr
               | StringLit String
               | Star
                 deriving (Eq,Show)

num :: Parser ValueExpr
num = NumLit <$> integer

iden :: [String] -> Parser ValueExpr
iden blacklist = Iden <$> identifierBlacklist blacklist

parensValue :: Parser ValueExpr
parensValue = Parens <$> parens (valueExpr [])

term :: [String] -> Parser ValueExpr
term blacklist = iden blacklist <|> num <|> parensValue <|> stringLit <|> star

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
          ,binaryK "like" E.AssocNone
          ,binary "!=" E.AssocRight
          ,binary "<>" E.AssocRight
          ,binary "||" E.AssocRight]
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

