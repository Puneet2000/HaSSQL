module BasicParsers
(valueExpr0 ,
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
                 deriving (Eq,Show)


lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

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

term0 :: Parser ValueExpr
term0 = iden <|> num <|> parensValue valueExpr0

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

valueExpr0 :: Parser ValueExpr
valueExpr0 = E.buildExpressionParser table term0

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







