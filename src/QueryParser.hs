{-|
Module      : QueryParser
Description : contains functions to parse SELECT query and evaluate it.
-}
module QueryParser (
QueryExpr(..),
queryExpr,
makeSelect,evaluateQuery) where

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

-- |'QueryExpr' represents a SELECT statement
data QueryExpr = Select
      {qeSelectList :: [(ValueExpr,Maybe String)] -- ^ 'qeSelectList' contains list of selected columns
      ,qefromClause :: ValueExpr -- ^ 'qeFromClause' contains table name
      ,qeWhere :: Maybe ValueExpr  -- ^ 'qeWhere' contains where clause expression
      ,qeGroupBy :: [ValueExpr] -- ^ 'qeGroupBy' contains list of groupby expresions
      ,qeHaving :: Maybe ValueExpr  -- ^ 'qeHaving' contains havingby expression
      ,qeOrderBy :: [ValueExpr] -- ^ 'qeOrderBy' contains list of orderby clause expressions
      } deriving (Eq,Show)

-- |'makeSelect' makes a empty 'QueryExpr' object
makeSelect :: QueryExpr
makeSelect = Select {qeSelectList = []
                    ,qefromClause = Iden ""
                    ,qeWhere = Nothing
                    ,qeGroupBy = []
                    ,qeHaving = Nothing
                    ,qeOrderBy = []}

-- |'selectList' parses comma seperated 'selectItem'
selectList :: Parser [(ValueExpr, Maybe String)]
selectList = keyword_ "select" *> commaSep1 selectItem

-- |'selectItem' parses selected list names and their alias
selectItem :: Parser (ValueExpr, Maybe String)
selectItem = (,) <$> valueExpr [] <*> optionMaybe (try alias)
  where alias = optional (keyword_ "as") *> identifierBlacklist ["from","where","group","having","order"]

-- |'fromClause' parses from gets table name
fromClause :: Parser ValueExpr
fromClause = keyword_ "from" *> iden ["from","where","group","having","order"]

-- |'whereClause' parses where clause
whereClause :: Parser ValueExpr
whereClause = keyword_ "where" *> valueExpr []

-- |'groupByClause' parses group by clauses as list
groupByClause :: Parser [ValueExpr]
groupByClause = keyword_ "group" *> keyword_ "by"
                *> commaSep1 (valueExpr [])

-- |'having' parses having by clause
having :: Parser ValueExpr
having = keyword_ "having" *> (valueExpr [])

-- |'orderBy' parses order by clauses as lists
orderByClause :: Parser [ValueExpr]
orderByClause = keyword_ "order" *> keyword_ "by"
          *> commaSep1 (valueExpr [])

-- |'queryExpr' aggregates all parsers to parse a SELECT statement
queryExpr :: Parser QueryExpr
queryExpr = Select
            <$> selectList
            <*> fromClause
            <*> optionMaybe whereClause
            <*> option [] groupByClause
            <*> optionMaybe having
            <*> option [] orderByClause

eval :: ValueExpr -> String
eval (Iden s) = s

-- |'evalWhere' evaluates a where clause
-- First argument is parsed where 'ValueExpr' 
-- Second argument is Maybe Database instance
-- Third arhument is table Name
evalWhere :: Maybe ValueExpr -> Maybe Database -> String -> [[(Int,String, Datatype, String)]]
evalWhere (Just expr) db tname = find (Right expr) db tname
evalWhere (Nothing) db tname = find (Right (BoolLit True)) db tname

-- |'evalOrderBy' evaluates a order by clause
-- First argument is parsed orderby 'ValueExpr' list
-- Second argument is output of where clause
evalOrderBy :: [ValueExpr]-> [[(Int,String, Datatype, String)]] -> [[(Int,String, Datatype, String)]]
evalOrderBy [] out = out
evalOrderBy (x : xs) out = evalOrderBy xs (orderBy (Right x) out)

-- |'evalSelect' converts select lists to String tuples
-- First argument is parsed select list
evalSelect :: [(ValueExpr, Maybe String)] -> [(String,String)]
evalSelect [(Star,Nothing)] = []
evalSelect [] = []
evalSelect (x : xs) = do
  case (snd x) of
    Nothing -> (eval (fst x),eval (fst x)) : evalSelect xs
    Just y -> (eval (fst x),y) : evalSelect xs

-- |'evaluateQuery' evaluates a query expression
-- First argument is parsed query expression
-- Second argument is Maybe Database instance
evaluateQuery :: Either ParseError QueryExpr -> Maybe Database -> [[(Int,String, Datatype, String)]]
evaluateQuery (Right expr) db = do 
  let out1 = evalWhere (qeWhere expr) db (eval (qefromClause expr))
  let out2 = evalOrderBy (qeOrderBy expr) out1
  select (evalSelect (qeSelectList expr)) out2



