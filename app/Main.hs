module Main where

import ExpressionParser
import Funcs
import CreateParser

main :: IO ()
main = do 
	let m = parseWithWSEof (valueExpr []) "(1+2)%3"
	print(evaluate m)
	print("hello")