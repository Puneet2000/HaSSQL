module InsertParser (
InsertExpr(..),
insertExpr,
makeInsert) where

import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Char 
import Text.ParserCombinators.Parsec.Combinator 
import Text.Parsec (parse , ParseError , try)
import Control.Applicative ((<*),(<$>), (*>), (<|>),(<$),(<*>))
import Control.Monad
import qualified Text.ParserCombinators.Parsec.Expr as E

import Data.Maybe ()
import Funcs
import ExpressionParser

data InsertExpr = Insert 
                { iTable :: ValueExpr 
                , iColumns :: [ValueExpr]
                , iValues :: [ValueExpr]
                } deriving(Eq,Show)

makeInsert :: InsertExpr
makeInsert =  Insert {iTable = Iden ""
                     ,iColumns = []
                     ,iValues = []}

tableName :: Parser ValueExpr
tableName = keyword_ "insert" *> keyword_ "into" *> iden []

columns :: Parser [ValueExpr]
columns = parens (commaSep1 (iden []))

values :: Parser [ValueExpr]
values =  keyword_ "values" *> parens (commaSep1 literal)
 where literal = num <|> stringLit

insertExpr :: Parser InsertExpr
insertExpr =  Insert <$> tableName <*> option [] columns <*> values

