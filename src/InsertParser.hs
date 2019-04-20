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
 where literal = num <|> stringLit <|> boolean

insertExpr :: Parser InsertExpr
insertExpr =  Insert <$> tableName <*> option [] columns <*> values

evaluateInsert :: Either ParseError InsertExpr -> Maybe Database -> Maybe Database
evaluateInsert (Right expr) db 
 | (iColumns expr == []) =  insertDefault (getValues (iValues expr)) db (eval (iTable expr))
 | otherwise = insert (getColumnNames (iColumns expr)) (getValues (iValues expr)) (getDataType (iValues expr)) db (eval (iTable expr))
 where eval (Iden s) = s

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


