module Main where

import ExpressionParser
import Funcs
import CreateParser
import Database

import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
-- main = print (regularParse createExpr "create table table1 ( c1 INTEGER , c2 INTEGER )")
main = do
    sampleCommands
    print "Hello, World!"
	let m = parseWithWSEof (valueExpr []) "a or (x>=y) or (z<=m)"
	print(m)
	let map0 = Map.empty
	let map1 = Map.insert "a" (BoolLit False) map0
	let map2 = Map.insert "x" (NumLit 1) map1
	let map3 = Map.insert "y" (NumLit 2) map2
	let map4 = Map.insert "z" (NumLit 2) map3
	let map5 = Map.insert "m" (NumLit 1) map4
	print (evaluate2 map5 m)
