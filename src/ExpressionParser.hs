{-|
Module      : ExpressionParser
Description : contains functions to parse expressions and evaluate them.
-}
module ExpressionParser where

import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Char 
import Text.ParserCombinators.Parsec.Combinator 
import Text.Parsec (parse , ParseError , try)
import Control.Applicative (many, (<*),(<$>), (*>), (<|>),(<$),(<*>))
import Control.Monad (void,guard)
import qualified Text.ParserCombinators.Parsec.Expr as E
import Data.Map (Map)
import qualified Data.Map as Map
import Funcs

-- |'ValueExpr' represnts a expression
data ValueExpr =  NumLit Integer -- ^ 'NumLit' type constructor for integer constants that takes integer as argument
               | Iden String -- ^ 'Iden' type constructor for identifier that takes string as argument
               | PrefOp String ValueExpr -- ^ 'PrefOp' for prefix operators that takes operator(String) and a 'ValueExpr'
               | BinOp ValueExpr String ValueExpr -- ^ 'BinOp' for binary operators that takes operator(String) and two 'ValueExpr'
               | Parens ValueExpr -- ^ 'Parens' type constructor that 'ValueExpr' as argument that is ought to be in parenthesis.
               | StringLit String -- ^ 'StringLit' type constructor for string literals that takes string as argument
               | Star -- ^ 'Star' for * symbol
               | BoolLit Bool -- ^ 'BoolLit' type constructor for boolean constants that takes bool as argument
                 deriving (Eq,Show)

-- |'EvalType' represnts a return type of evaluator
data EvalType = Int Integer -- ^ 'Int' holds am integral return type
              | Boolean Bool -- ^ 'Boolean' holds a boolean return type
              | Error String -- ^ 'Error' holds a error string
                deriving (Eq,Show)

-- |'num' wraps a parsed integer literal in 'ValueExpr' datatype
num :: Parser ValueExpr
num = NumLit <$> integer

-- |'iden' wraps a parsed identifier literal in 'ValueExpr' datatype
-- Argument is a blacklist
iden :: [String] -> Parser ValueExpr
iden blacklist = Iden <$> identifierBlacklist (blacklist++["True","False","not","and","or"])

-- |'bolean' wraps a parsed boolean literal in 'ValueExpr' datatype
boolean :: Parser ValueExpr
boolean =  BoolLit <$> boolToken

-- |'parensValue' wraps a parsed parenthesis expression in 'ValueExpr' datatype
parensValue :: Parser ValueExpr
parensValue = Parens <$> parens (valueExpr [])

-- |'term' contains all possible expression types
term :: [String] -> Parser ValueExpr
term blacklist = try boolean <|> iden blacklist  <|> num <|> parensValue <|> stringLit <|> star

-- |'table' contains precedence, associativity of integer and boolean operators
table = [[prefix "-", prefix "+"]
         ,[binary "^" E.AssocLeft]
         ,[binary "*" E.AssocLeft
          ,binary "/" E.AssocLeft
          ,binary "%" E.AssocLeft]
         ,[binary "+" E.AssocLeft
          ,binary "-" E.AssocLeft]
         ,[binary "<=" E.AssocRight
          ,binary ">=" E.AssocRight
          ,binary "!=" E.AssocRight]
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

-- |'valueExpr' parses a given string to 'ValueExpr' datatype
-- Input is a blacklist
valueExpr :: [String] -> Parser ValueExpr
valueExpr blacklist = E.buildExpressionParser table (term blacklist)

-- |'stringLit' wraps a parsed string literal in 'ValueExpr' datatype
stringLit :: Parser ValueExpr
stringLit = StringLit <$> stringToken

-- |'star' wraps a parsed '*' keyword in 'ValueExpr' datatype
star :: Parser ValueExpr
star = Star <$ symbol "*"

-- |'evaluate' evaluates a parsed 'ValueExpr' to integer 'EvalType' if possible otherwise Exception
-- First argument is map of variables and corresponding values.
-- Second argument is a parsed expression type.
evaluate :: (Map String ValueExpr) -> Either ParseError ValueExpr -> EvalType
evaluate map (Right e) = Int (evaluateExpr map e)
evaluate map (Left e) = Error (show e)

-- |'evaluate2' evaluates a parsed 'ValueExpr' to boolean 'EvalType'if possible otherwise Exception
-- First argument is map of variables and corresponding values.
-- Second argument is a parsed expression type.
evaluate2 :: (Map String ValueExpr) -> Either ParseError ValueExpr -> EvalType
evaluate2 map (Right e) = Boolean (evaluateExpr2 map e)
evaluate2 map (Left e) = Error (show e)

-- |'evaluateExpr' evaluates a 'ValueExpr' to Integer if possible otherwise Exception
-- First argument is map of variables and corresponding values.
-- Second argument is a parsed expression ('ValueExpr') type.
evaluateExpr :: (Map String ValueExpr) -> ValueExpr -> Integer
evaluateExpr map (Iden s) = do
 let val = Map.lookup s map
 case val of
  Just (NumLit i) -> i
evaluateExpr map (NumLit n) = n
evaluateExpr map (Parens v) = evaluateExpr map v
evaluateExpr map (PrefOp "-" v) = -(evaluateExpr map v)
evaluateExpr map (PrefOp "+" v) = (evaluateExpr map v)
evaluateExpr map (BinOp v1 "^" v2) = (evaluateExpr map v1)^(evaluateExpr map v2)
evaluateExpr map (BinOp v1 "*" v2) = (evaluateExpr map v1)*(evaluateExpr map v2)
evaluateExpr map (BinOp v1 "/" v2) = div (evaluateExpr map v1) (evaluateExpr map v2)
evaluateExpr map (BinOp v1 "%" v2) = mod (evaluateExpr map v1) (evaluateExpr map v2)
evaluateExpr map (BinOp v1 "+" v2) = (evaluateExpr map v1) + (evaluateExpr map v2)
evaluateExpr map (BinOp v1 "-" v2) = (evaluateExpr map v1) - (evaluateExpr map v2)

-- |'evaluateExpr2' evaluates a 'ValueExpr' to Bool if possible otherwise Exception
-- First argument is map of variables and corresponding values.
-- Second argument is a parsed expression ('ValueExpr') type.
evaluateExpr2 :: (Map String ValueExpr) -> ValueExpr -> Bool
evaluateExpr2 map (Iden s) = do
 let val = Map.lookup s map
 case val of
  Just (BoolLit i) -> i
evaluateExpr2 map (BoolLit True) = True
evaluateExpr2 map (BoolLit False) = False
evaluateExpr2 map (Parens v) = evaluateExpr2 map v
evaluateExpr2 map (BinOp v1 "<=" v2) = (evaluateExpr map v1) <= (evaluateExpr map v2)
evaluateExpr2 map (BinOp v1 ">=" v2) = (evaluateExpr map v1) >= (evaluateExpr map v2)
evaluateExpr2 map (BinOp v1 "!=" v2) = (evaluateExpr map v1) /= (evaluateExpr map v2)
evaluateExpr2 map (BinOp v1 ">" v2) = (evaluateExpr map v1) > (evaluateExpr map v2)
evaluateExpr2 map (BinOp v1 "<" v2) = (evaluateExpr map v1) < (evaluateExpr map v2)
evaluateExpr2 map (BinOp v1 "=" v2) = (evaluateExpr map v1) == (evaluateExpr map v2)
evaluateExpr2 map (PrefOp "not" v) = not (evaluateExpr2 map v)
evaluateExpr2 map (BinOp v1 "and" v2) = (&&) (evaluateExpr2 map v1) (evaluateExpr2 map v2)
evaluateExpr2 map (BinOp v1 "or" v2) = (||) (evaluateExpr2 map v1) (evaluateExpr2 map v2)

