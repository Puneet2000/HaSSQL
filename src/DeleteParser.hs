{-|
Module      : DeleteParser
Description : contains functions to parse DELETE query and evaluate it.
-}
module DeleteParser (
DeleteExpr(..),
deleteExpr,
makeDelete, evaluateDelete ) where

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

data DeleteExpr = Delete
                { dTname :: ValueExpr -- ^ 'iTname' is name of the table
                , dWhere :: ValueExpr  -- ^ 'iWhere' contains where clause expression
                } deriving(Eq,Show)

-- |'makeDelete' creates empty 'DeleteExpr' object
makeDelete :: DeleteExpr
makeDelete =  Delete {dTname = Iden ""
                     ,dWhere = BoolLit False
                 }

-- |'tabeName' parses name of the table
tableName :: Parser ValueExpr
tableName = keyword_ "delete" *> keyword_ "from" *> iden []

-- |'whereClause' parses where clause
whereClause :: Parser ValueExpr
whereClause = keyword_ "where" *> valueExpr []

-- |deleteExpr' aggerates all parsers to parse CREATE query
deleteExpr :: Parser DeleteExpr
deleteExpr = Delete <$> tableName <*> whereClause

eval :: ValueExpr -> String
eval (Iden s) = s

-- |'evaluateDelete' evaluates parsed 'DeleteExpr' to delete rows in database table
-- First argument is parsed 'DeleteExpr'
-- Second argument is databse instance
evaluateDelete:: Either ParseError DeleteExpr -> Maybe Database -> Maybe Database
evaluateDelete (Right expr) db = delete (Right (dWhere expr)) db (eval (dTname expr))