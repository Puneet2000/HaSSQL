module BasicParsers
(valueExpr ,
 ValueExpr(..),
 whitespace) where

import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Char (oneOf, digit, string, anyChar, char, letter)
import Text.ParserCombinators.Parsec.Combinator (many1, manyTill, eof, choice, between
                                     ,sepBy, optionMaybe)
import Text.Parsec (parse , ParseError , try)
import Control.Applicative (many, (<*),(<$>), (*>), (<|>),(<$),(<*>))
import Control.Monad (void,guard)
import qualified Text.ParserCombinators.Parsec.Expr as E

data ValueExpr =  NumLit Integer
               | Iden String
               | PrefOp String ValueExpr
               | BinOp ValueExpr String ValueExpr
               | Parens ValueExpr
               | Case (Maybe ValueExpr) [(ValueExpr,ValueExpr)] (Maybe ValueExpr)
               | StringLit String
               | Star
                 deriving (Eq,Show)

-- Lexeme parser : consumes all whitespaces after parsing
lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

-- integer parsing : consumes integer
integer :: Parser Integer
integer =  read <$> lexeme (many1 digit)

identifier :: Parser String
identifier = lexeme ((:) <$> firstChar <*> many nonFirstChar)
      where
            firstChar = letter <|> char '_'
            nonFirstChar =  digit <|> firstChar


symbol :: String -> Parser String
symbol s = try $ lexeme $ do
    u <- many1 (oneOf "<>=+-^%/*!|")
    guard (s == u)
    return s

openParen :: Parser Char
openParen = lexeme $ char '('

closeParen :: Parser Char
closeParen = lexeme $ char ')'

keyword :: String -> Parser String
keyword k = try $ do
    i <- identifier
    guard (i == k)
    return k

parens :: Parser a -> Parser a
parens = between openParen closeParen

num :: Parser ValueExpr
num = NumLit <$> integer

iden :: Parser ValueExpr
iden = Iden <$> identifier

parensValue :: Parser ValueExpr -> Parser ValueExpr
parensValue val = Parens <$> parens val

term :: Parser ValueExpr
term = caseValue valueExpr <|> iden <|> num <|> parensValue valueExpr

--table :: [[E.Operator ValueExpr]]
table = [[prefix "-", prefix "+"]
         ,[binary "^" E.AssocLeft]
         ,[binary "*" E.AssocLeft
          ,binary "/" E.AssocLeft
          ,binary "%" E.AssocLeft]
         ,[binary "+" E.AssocLeft
          ,binary "-" E.AssocLeft]
         ,[binary "<=" E.AssocRight
          ,binary ">=" E.AssocRight
          ,binaryK "like" E.AssocNone
          ,binary "!=" E.AssocRight
          ,binary "<>" E.AssocRight
          ,binary "||" E.AssocRight]
         ,[binary "<" E.AssocNone
          ,binary ">" E.AssocNone]
         ,[binary "=" E.AssocRight]
         ,[prefixK "not"]
         ,[binaryK "and" E.AssocLeft]
         ,[binaryK "or" E.AssocLeft]]
  where
    binary name assoc =
        E.Infix (mkBinOp name <$ symbol name) assoc
    mkBinOp nm a b = BinOp a nm b
    prefix name = E.Prefix (PrefOp name <$ symbol name)
    binaryK name assoc =
        E.Infix (mkBinOp name <$ keyword name) assoc
    prefixK name = E.Prefix (PrefOp name <$ keyword name)

valueExpr :: Parser ValueExpr
valueExpr = E.buildExpressionParser table term

lineComment :: Parser ()
lineComment = void (try (string "--") *>
                    manyTill anyChar (void (char '\n') <|> eof))

blockComment :: Parser ()
blockComment = void (try (string "/*") *> manyTill anyChar (try $ string "*/"))

whitespace :: Parser ()
whitespace =
    choice [simpleWhitespace *> whitespace
           ,lineComment *> whitespace
           ,blockComment *> whitespace
           ,return ()]
  where
    lineComment = try (string "--")
                  *> manyTill anyChar (void (char '\n') <|> eof)
    blockComment = try (string "/*")
                   *> manyTill anyChar (try $ string "*/")
    simpleWhitespace = void $ many1 (oneOf " \t\n")



blackListValueExpr :: [String] -> Parser ValueExpr -> Parser ValueExpr
blackListValueExpr blacklist val = try $ do
  v <- val
  guard $ case v of 
    Iden i | i `elem` blacklist -> False
    _ -> True
  return v

caseValue :: Parser ValueExpr -> Parser ValueExpr
caseValue val = do
  void $ keyword "case"
  testExp <- optionMaybe caseVal
  whens <- many1 whenClause
  els <- optionMaybe elseClause
  void $ keyword "end"
  return $ Case testExp whens els
  where
    whenClause = do
      void $ keyword "when"
      w <- caseVal
      void $ keyword "then"
      t <- caseVal
      return (w,t)
    elseClause = do
      void $ keyword "else"
      v <- caseVal
      return v
    caseVal = blackListValueExpr blacklist val
    blacklist = ["case" , "when" ,"then" , "else" , "end"]



