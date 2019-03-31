module Main where

import BasicParsers
import Funcs

main :: IO ()
main = print (regularParse (valueExpr []) "case a when (a>=b) then a=1 else a=2 end")