module Main where

import ExpressionParser
import Funcs
import CreateParser

main :: IO ()
main = print (regularParse createExpr "create table table1 ( c1 INTEGER , c2 STRING , c3 BOOL )")