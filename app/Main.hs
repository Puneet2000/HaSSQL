module Main where

import ExpressionParser
import Funcs
import InsertParser

main :: IO ()
main = print (regularParse insertExpr "insert into table1 values (1,'Hello')")