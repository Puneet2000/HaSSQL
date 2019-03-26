module Main where

import BasicParsers
import Funcs

main :: IO ()
main = print (regularParse valueExpr0 "a+b+c")
