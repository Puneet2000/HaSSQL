module SQLParser where
import Text.Parsec.String (Parser)
import Text.Parsec.Error (ParseError)
import Text.Parsec (parse )
import Text.ParserCombinators.Parsec.Combinator (eof)
import Control.Applicative ((<*),(<$>), (*>), (<|>),(<$),(<*>))
import Funcs
import ExpressionParser
import QueryParser
import InsertParser
import CreateParser
import Database
import Data.Map (Map)
import qualified Data.Map as Map

data SQLExpr = SELECT QueryExpr | INSERT InsertExpr | CREATE CreateExpr deriving (Eq, Show)
data ResType = DB (Maybe Database) | OUT [[(String, Datatype, String)]] | ERROR String

sqlExpr ::  Parser SQLExpr
sqlExpr = (SELECT <$> queryExpr ) <|> (INSERT <$> insertExpr) <|> (CREATE <$> createExpr)

evaluateSQL :: Either ParseError SQLExpr -> Maybe Database -> ResType
evaluateSQL (Right expr)  db = do 
    case expr of 
        SELECT e -> OUT <$> (evaluateQuery e db)
        INSERT e -> DB <$> (evaluateInsert e db)
        CREATE e -> DB <$> (evaluateCreate e db)
evaluateSQL (Left error) db = ERROR <$> show error


