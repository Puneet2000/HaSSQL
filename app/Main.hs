module Main where

import ExpressionParser
import Funcs
import CreateParser
import Database
import InsertParser

import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
-- main = print (regularParse createExpr "create table table1 ( c1 INTEGER , c2 INTEGER )")
main = do
    let cr = regularParse createExpr "create table table1 ( c1 INTEGER , c2 STRING , c3 BOOL)"
    let mdb = newDatabase "mdb1" Map.empty
    let mdb2 = evaluateCreate cr mdb
    print(mdb2)
    let ins = regularParse insertExpr "insert into table1 (c1,c2,c3) values (1,'Hello',True)"
    let mdb3 = evaluateInsert ins mdb2
    let ins = regularParse insertExpr "insert into table1 (c1,c2,c3) values (2,'Puneet',False)"
    let mdb4 = evaluateInsert ins mdb3
    print(mdb4)
    -- let m = parseWithWSEof (valueExpr []) "a or (x>=y) or (z<=m)"
    -- print(m)
    -- let map0 = Map.empty
    -- let map1 = Map.insert "a" (BoolLit False) map0
    -- let map2 = Map.insert "x" (NumLit 1) map1
    -- let map3 = Map.insert "y" (NumLit 2) map2
    -- let map4 = Map.insert "z" (NumLit 2) map3
    -- let map5 = Map.insert "m" (NumLit 1) map4
    -- print (evaluate2 map5 m)
