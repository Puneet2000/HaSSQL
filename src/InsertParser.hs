{-|
Module      : InsertParser
Description : contains functions to parse INSERT query and evaluate it.
-}
module InsertParser (
InsertExpr(..),
insertExpr,
makeInsert,evaluateInsert) where

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

data InsertExpr = Insert 
                { iTable :: ValueExpr -- ^ 'iTname' is name of the table
                , iColumns :: [ValueExpr] -- ^ 'iColumns' are name of columns
                , iValues :: [ValueExpr] -- ^ 'iValues' are values to insert (NumLit, StringLit, BoolLit)
                } deriving(Eq,Show)

-- |'makeInsert' creates empty 'InsertExpr' object
makeInsert :: InsertExpr
makeInsert =  Insert {iTable = Iden ""
                     ,iColumns = []
                     ,iValues = []}

-- |'tabeName' parses name of the table
tableName :: Parser ValueExpr
tableName = keyword_ "insert" *> keyword_ "into" *> iden []

-- |'columns' parses column name as list
columns :: Parser [ValueExpr]
columns = parens (commaSep1 (iden []))

-- |'values' parses column values as list
values :: Parser [ValueExpr]
values =  keyword_ "values" *> parens (commaSep1 literal)
 where literal = num <|> stringLit <|> boolean

-- |'insertExpr' aggerates all parsers to parse INSERT query
insertExpr :: Parser InsertExpr
insertExpr =  Insert <$> tableName <*> option [] columns <*> values

-- |'evaluateInsert' evaluates insert expression ans insert values to table
-- First argument is InserExpr
-- Second argument is database instance
evaluateInsert :: Either ParseError InsertExpr -> Maybe Database -> Maybe Database
evaluateInsert (Right expr) db 
 | (iColumns expr == []) =  insertDefault (getValues (iValues expr)) (getDataType (iValues expr)) db (eval (iTable expr))
 | otherwise = insert (getColumnNames (iColumns expr)) (getValues (iValues expr)) (getDataType (iValues expr)) db (eval (iTable expr))
 where eval (Iden s) = s

{-|
	Utility functions
-}
getDataType :: [ValueExpr] -> [Datatype]
getDataType [] = []
getDataType ((StringLit _) : xs) = STRING : getDataType xs
getDataType ((NumLit _) : xs) = INT : getDataType xs
getDataType ((BoolLit _) : xs) = BOOL : getDataType xs

getColumnNames :: [ValueExpr] -> [String]
getColumnNames [] = []
getColumnNames ((Iden s) : xs) = s : getColumnNames xs

getValues :: [ValueExpr] -> [String]
getValues [] = []
getValues ((StringLit s) : xs ) = s : getValues xs
getValues ((BoolLit b) : xs ) = (show b) : getValues xs
getValues ((NumLit i) : xs ) = (show i) : getValues xs