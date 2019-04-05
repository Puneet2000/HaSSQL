module CreateParser (
CreateExpr(..),
createExpr,
makeCreate) where

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

data DtypeExpr = Integer | String | Bool deriving(Eq,Show)

int :: Parser DtypeExpr 
int = Integer <$ keyword "INTEGER"

str :: Parser DtypeExpr
str =  String <$ keyword "STRING"

bool :: Parser DtypeExpr
bool = Bool <$ keyword "BOOL"

dtypeExpr :: Parser DtypeExpr
dtypeExpr = int <|> str <|> bool

data CreateExpr = Create
                { iTable :: ValueExpr 
                , iColumns :: [(ValueExpr , DtypeExpr)]
                } deriving(Eq,Show)

makeCreate :: CreateExpr
makeCreate =  Create {iTable = Iden ""
                     ,iColumns = []
                 }

tableName :: Parser ValueExpr
tableName = keyword_ "create" *> keyword_ "table" *> iden []

column :: Parser (ValueExpr, DtypeExpr)
column = (,) <$> iden [] <*> dtypeExpr

columnList :: Parser [(ValueExpr,DtypeExpr)]
columnList = parens (commaSep1 column)

createExpr :: Parser CreateExpr
createExpr = Create <$> tableName <*> columnList