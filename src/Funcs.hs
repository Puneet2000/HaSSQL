module Funcs where 
import Text.Parsec.String (Parser)
import Text.Parsec (parse , ParseError , try)
import Text.ParserCombinators.Parsec.Char (oneOf, digit, string, anyChar, char, letter)
import Text.ParserCombinators.Parsec.Combinator (many1, manyTill, anyToken, eof, choice, between
                                     ,sepBy,sepBy1, optionMaybe)
import Control.Applicative (many, (<*),(<$>), (*>), (<|>),(<$),(<*>))
import Control.Monad (void,guard)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

parseWithLeftOver :: Parser a -> String -> Either ParseError (a,String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
  where leftOver = manyTill anyToken eof

parseWithWSEof :: Parser a -> String -> Either ParseError a
parseWithWSEof p = parseWithEof (whiteSpace *> p)
  where whiteSpace = void $ many $ oneOf " \n\t"

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

comma :: Parser Char
comma = lexeme $ char ','

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

identifierBlacklist :: [String] -> Parser String
identifierBlacklist bl = do
    i <- identifier
    guard (i `notElem` bl)
    return i

whitespace :: Parser ()
whitespace = void $ many (oneOf " \t\n")

keyword_ :: String -> Parser ()
keyword_ = void . keyword

symbol_ :: String -> Parser ()
symbol_ = void . symbol

commaSep1 :: Parser a -> Parser [a]
commaSep1 = (`sepBy1` comma)

stringToken :: Parser String
stringToken = lexeme (char '\'' *> manyTill anyChar (char '\''))

