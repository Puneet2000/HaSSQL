{-|
Module      : QueryParser
Description : contains functions to parse SELECT query and evaluate it.
-}
module QueryParser (
QueryExpr(..),
queryExpr,
makeSelect) where

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

-- |'QueryExpr' represents a SELECT statement
data QueryExpr = Select
      {qeSelectList :: [(ValueExpr,Maybe String)] -- ^ 'qeSelectList' contains list of selected columns
      ,qefromClause :: ValueExpr
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
orderBy :: Parser [ValueExpr]
orderBy = keyword_ "order" *> keyword_ "by"
          *> commaSep1 (valueExpr [])

-- |'queryExpr' aggregates all parsers to parse a SELECT statement
queryExpr :: Parser QueryExpr
queryExpr = Select
            <$> selectList
            <*> fromClause
            <*> optionMaybe whereClause
            <*> option [] groupByClause
            <*> optionMaybe having
            <*> option [] orderBy


