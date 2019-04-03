module Main where

import ExpressionParser
import Funcs

main :: IO ()
main = print (regularParse (valueExpr []) "1+2+3")