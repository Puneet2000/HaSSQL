module Main where

import ExpressionParser
import Funcs
import CreateParser
import Database

main :: IO ()
-- main = print (regularParse createExpr "create table table1 ( c1 INTEGER , c2 INTEGER )")
main = do
    sampleCommands
    print "Hello, World!"