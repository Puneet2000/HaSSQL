module CreateParser (
CreateExpr(..),
createExpr,
makeCreate, evaluateCreate ) where

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
import Database

int :: Parser Datatype 
int = INT <$ keyword "INTEGER"

str :: Parser Datatype
str =  STRING <$ keyword "STRING"

bool :: Parser Datatype
bool = BOOL <$ keyword "BOOL"

dtypeExpr :: Parser Datatype
dtypeExpr = int <|> str <|> bool

data CreateExpr = Create
                { iTname :: ValueExpr 
                , iColLists:: [(ValueExpr , Datatype)]
                } deriving(Eq,Show)

makeCreate :: CreateExpr
makeCreate =  Create {iTname = Iden ""
                     ,iColLists = []
                 }

tableName :: Parser ValueExpr
tableName = keyword_ "create" *> keyword_ "table" *> iden []

column :: Parser (ValueExpr, Datatype)
column = (,) <$> iden [] <*> dtypeExpr

columnList :: Parser [(ValueExpr,Datatype)]
columnList = parens (commaSep1 column)

createExpr :: Parser CreateExpr
createExpr = Create <$> tableName <*> columnList

addColumnList :: [(ValueExpr ,Datatype)] -> Maybe Database -> String -> Maybe Database
addColumnList [] db tb = db
addColumnList (x : xs) db tb = addColumnList xs (addColumn (eval (fst x)) (snd x) db tb) tb

eval :: ValueExpr -> String
eval (Iden s) = s

evaluateCreate :: Either ParseError CreateExpr -> Maybe Database -> Maybe Database
evaluateCreate (Right expr) db = do
 let db_1 = addNewTable (eval (iTname expr)) db
 addColumnList (iColLists expr) db_1 (eval (iTname expr))

