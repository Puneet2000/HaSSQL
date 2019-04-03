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

data QueryExpr = Select
      {qeSelectList :: [(ValueExpr,Maybe String)]
      ,qeWhere :: Maybe ValueExpr
      ,qeGroupBy :: [ValueExpr]
      ,qeHaving :: Maybe ValueExpr
      ,qeOrderBy :: [ValueExpr]
      } deriving (Eq,Show)

makeSelect :: QueryExpr
makeSelect = Select {qeSelectList = []
                    ,qeWhere = Nothing
                    ,qeGroupBy = []
                    ,qeHaving = Nothing
                    ,qeOrderBy = []}

selectList :: Parser [(ValueExpr, Maybe String)]
selectList = keyword_ "select" *> commaSep1 selectItem

selectItem :: Parser (ValueExpr, Maybe String)
selectItem = (,) <$> valueExpr [] <*> optionMaybe (try alias)
  where alias = optional (keyword_ "as") *> identifierBlacklist ["from","where","group","having","order"]

whereClause :: Parser ValueExpr
whereClause = keyword_ "where" *> valueExpr []

groupByClause :: Parser [ValueExpr]
groupByClause = keyword_ "group" *> keyword_ "by"
                *> commaSep1 (valueExpr [])

having :: Parser ValueExpr
having = keyword_ "having" *> (valueExpr [])

orderBy :: Parser [ValueExpr]
orderBy = keyword_ "order" *> keyword_ "by"
          *> commaSep1 (valueExpr [])

queryExpr :: Parser QueryExpr
queryExpr = Select
            <$> selectList
            <*> optionMaybe whereClause
            <*> option [] groupByClause
            <*> optionMaybe having
            <*> option [] orderBy


