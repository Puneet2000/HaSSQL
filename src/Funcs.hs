{-|
Module      : Funcs
Description : Contains standard parsers fo building up other parsers
-}
module Funcs where 
import Text.Parsec.String (Parser)
import Text.Parsec (parse , ParseError , try)
import Text.ParserCombinators.Parsec.Char (oneOf, digit, string, anyChar, char, letter)
import Text.ParserCombinators.Parsec.Combinator (many1, manyTill, anyToken, eof, choice, between
                                     ,sepBy,sepBy1, optionMaybe)
import Control.Applicative (many, (<*),(<$>), (*>), (<|>),(<$),(<*>))
import Control.Monad (void,guard)

-- |The 'regularParse' parses a expression regularlly
regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

-- |The 'parseWithEof' is a wrapper that throws error if you haven’t consumed all the input
parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

-- |The 'parseWithLeftOver' is wrapper that tell you what was not consumed from the input by input parser
parseWithLeftOver :: Parser a -> String -> Either ParseError (a,String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
  where leftOver = manyTill anyToken eof

-- |The 'parseWithWSEof' wrapper allows parsing when input constains and leading whitespaces
parseWithWSEof :: Parser a -> String -> Either ParseError a
parseWithWSEof p = parseWithEof (whiteSpace *> p)
  where whiteSpace = void $ many $ oneOf " \n\t"

-- |The 'lexeme' is wrapper parser that eats up any following whitespaces after parsing.
lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

-- |The 'integer' parses a integer constant
integer :: Parser Integer
integer =  read <$> lexeme (many1 digit)

-- |The 'identifier' parses a identifier string
identifier :: Parser String
identifier = lexeme ((:) <$> firstChar <*> many nonFirstChar)
      where
            firstChar = letter <|> char '_'
            nonFirstChar =  digit <|> firstChar

-- |The 'symbol' parses a single character symbol or operator
-- It takes string as arguments and comapres it with parsed string.
symbol :: String -> Parser String
symbol s = try $ lexeme $ do
    u <- many1 (oneOf "<>=+-^%/*!|")
    guard (s == u)
    return s

-- |The 'comma' parses comma
comma :: Parser Char
comma = lexeme $ char ','

-- |The 'openParen' parses Opening paranthesis
openParen :: Parser Char
openParen = lexeme $ char '('

-- |The 'closeParen' parses closing paranthesis
closeParen :: Parser Char
closeParen = lexeme $ char ')'

-- |The 'keyowrd' parses a occurence of string given as input
-- It takes a string to pe parsed as only input
keyword :: String -> Parser String
keyword k = try $ do
    i <- identifier
    guard (i == k)
    return k

-- |The 'parens' function parses anything between opening and closing paranthesis
parens :: Parser a -> Parser a
parens = between openParen closeParen

-- |The 'identifierBlacklist' parses only that identifiers that are not in blacklist
-- It takes a blacklist of type List as an argument
identifierBlacklist :: [String] -> Parser String
identifierBlacklist bl = do
    i <- identifier
    guard (i `notElem` bl)
    return i

-- |The 'whitespace' parses any number of any type of whiespaces
whitespace :: Parser ()
whitespace = void $ many (oneOf " \t\n")

-- |The 'keyword_' parses keywords and ignores them
keyword_ :: String -> Parser ()
keyword_ = void . keyword

-- |The 'symbol_' parses symbol and ignores them
symbol_ :: String -> Parser ()
symbol_ = void . symbol

-- |The 'commaSep1' parses comma seperated items and returns a list
commaSep1 :: Parser a -> Parser [a]
commaSep1 = (`sepBy1` comma)

-- |The 'stringToken' parses a string literal in single quotes
stringToken :: Parser String
stringToken = lexeme (char '\'' *> manyTill anyChar (char '\''))

-- |The 'boolToken' parses boolean constants 'True' and 'False'
boolToken :: Parser Bool
boolToken = do
  i <- identifier
  guard (i `elem` ["True","False"])
  if i == "True" 
    then return True 
  else return False
