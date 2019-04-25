{-
module:      SQLParser
Description: Integrate all SQL statements into a single module
-}
module SQLParser where
import Text.Parsec.String (Parser)
import Text.Parsec.Error (ParseError)
import Text.Parsec (parse)
import Text.ParserCombinators.Parsec.Combinator (eof)
import Control.Applicative ((<*),(<$>), (*>), (<|>),(<$),(<*>))
import Funcs
import ExpressionParser
import QueryParser
import InsertParser
import CreateParser
import DeleteParser
import Database
import Data.Map (Map)
import qualified Data.Map as Map

-- | 'SQLExpr' is a wrapping type constructor for Select, Insert and Delete statements.
-- 'SELECT QueryExpr' contains 
data SQLExpr = SELECT QueryExpr -- ^Parses a SELECT query 
               | INSERT InsertExpr  -- ^ Parses a INSERT Query
               | CREATE CreateExpr -- ^ Parses a CREATE Query
               | DELETE DeleteExpr -- ^ Parses a DELETE Query
               deriving (Eq, Show)

-- | 'ResType' is a wrapping type constructor for the response types of each of the above statements.
data ResType = DB (Maybe Database) -- ^ Database instance for insert, create and delete queries
              | OUT ([[(String, Datatype, String)]]) -- ^ Return object for SELECT queries
              | ERROR String deriving(Eq,Show) -- ^ Error type

-- | 'sqlExpr' describes an SQL expression.
sqlExpr ::  Parser SQLExpr
sqlExpr = (SELECT <$> queryExpr ) <|> (INSERT <$> insertExpr) <|> (CREATE <$> createExpr) <|> (DELETE <$> deleteExpr)

-- | 'evaluateSQL' evaluates each Expression using individual case statements
evaluateSQL :: Either ParseError SQLExpr -> ResType -> ResType
evaluateSQL (Right expr)  (DB db) = do 
    case expr of 
        SELECT e -> OUT (evaluateQuery (Right e) db)
        INSERT e -> DB (evaluateInsert (Right e) db)
        CREATE e -> DB (evaluateCreate (Right e) db)
        DELETE e -> DB (evaluateDelete (Right e) db)
evaluateSQL (Left error) db = ERROR (show error)

-- |'getHeader' returns string of headers of table
-- First argument is list of headers
getHeaders :: [(String,Datatype,String)] -> String
getHeaders [] = ""
getHeaders ((a,b,c) : xs) = (a++" ("++(show b)++")    ")++(getHeaders xs)

-- |'getHeader' returns string of depicting one entire row
-- First argument is a row
getRow :: [(String,Datatype,String)] -> String
getRow [] = ""
getRow ((a,b,c) : xs) = (c++"          ")++(getRow xs)

-- |'getRows' prints the rows which are the output of select query
-- The argument is output of select parsed query
getRows :: [[(String,Datatype,String)]] -> IO ()
getRows [] = putStr ""
getRows (x: xs) =  putStrLn(getRow x) *> (getRows xs)

-- |'queryPrinter' prints the Select query output in a tabular fashion
-- The argument is output of select parsed query
queryPrinter :: ResType -> IO ()
queryPrinter (OUT output) =  do
  case output of
    [] -> print("")
    otherwise -> putStrLn("------------------------------------------------------------")
                 *> putStrLn(getHeaders (head output))
                 *> putStrLn("------------------------------------------------------------")
                 *> (getRows output)






