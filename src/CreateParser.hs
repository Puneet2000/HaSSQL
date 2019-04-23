{-|
Module      : CreateParser
Description : contains functions to parse CREATE query and evaluate it.
-}
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

-- |'int' parses INTEGER datatype
int :: Parser Datatype 
int = INT <$ keyword "INTEGER"

-- |'str' parses STRING datatype
str :: Parser Datatype
str =  STRING <$ keyword "STRING"

-- |'bool' parses BOOL datatype
bool :: Parser Datatype
bool = BOOL <$ keyword "BOOL"

-- |'dtypeExpr' parses Datatypes
dtypeExpr :: Parser Datatype
dtypeExpr = int <|> str <|> bool

data CreateExpr = Create
                { iTname :: ValueExpr -- ^ 'iTname' is name of the table
                , iColLists:: [(ValueExpr , Datatype)] -- ^ 'iColLists' contains list of column names and their dataype values
                } deriving(Eq,Show)

-- |'makeCreate' creates empty 'CreateExpr' object
makeCreate :: CreateExpr
makeCreate =  Create {iTname = Iden ""
                     ,iColLists = []
                 }

-- |'tabeName' parses name of the table
tableName :: Parser ValueExpr
tableName = keyword_ "create" *> keyword_ "table" *> iden []

-- |'column' parses column name and datatype as a tuple
column :: Parser (ValueExpr, Datatype)
column = (,) <$> iden [] <*> dtypeExpr

-- |'columnList' parses comma seperated columns as list
columnList :: Parser [(ValueExpr,Datatype)]
columnList = parens (commaSep1 column)

-- |'createExpr' aggerates all parsers to parse CREATE query
createExpr :: Parser CreateExpr
createExpr = Create <$> tableName <*> columnList

-- |'addColumnList' is a recursive function to add columns to table
-- First argument is column list (Name, Datatype)
-- Second argument is Maybe Database instance
-- Third argument is string (table name)
addColumnList :: [(ValueExpr ,Datatype)] -> Maybe Database -> String -> Maybe Database
addColumnList [] db tb = db
addColumnList (x : xs) db tb = addColumnList xs (addColumn (eval (fst x)) (snd x) db tb) tb

eval :: ValueExpr -> String
eval (Iden s) = s

-- |'evaluateCreate' evaluates parsed 'CreateExpr' to create a new table and add columns to it.
-- First argument is parsed 'CreateExpr'
-- Second argument is databse instance
evaluateCreate :: Either ParseError CreateExpr -> Maybe Database -> Maybe Database
evaluateCreate (Right expr) db = do
 let db_1 = addNewTable (eval (iTname expr)) db
 addColumnList (iColLists expr) db_1 (eval (iTname expr))

