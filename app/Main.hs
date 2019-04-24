module Main where

import ExpressionParser
import Funcs
import CreateParser
import Database
import InsertParser
import QueryParser

import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
-- main = print (regularParse createExpr "create table table1 ( c1 INTEGER , c2 INTEGER )")
main = do
    let cr = regularParse createExpr "create table table1 ( c1 INTEGER , c2 STRING , c3 BOOL)"
    let mdb = newDatabase "mdb1" Map.empty
    let mdb2 = evaluateCreate (regularParse createExpr "create table table1 ( c1 INTEGER , c2 STRING , c3 BOOL)") mdb
    let mdb3 = evaluateInsert (regularParse insertExpr "insert into table1 values (1,'Puneet',True)") mdb2
    let mdb4 = evaluateInsert (regularParse insertExpr "insert into table1 values (11,'Shraiysh',False)") mdb3
    let mdb5 = evaluateInsert (regularParse insertExpr "insert into table1 values (1,'Sai ramana',True)") mdb4
    let mdb6 = evaluateInsert (regularParse insertExpr "insert into table1 values (2,'Hitesh',False)") mdb5
    let mdb7 = evaluateInsert (regularParse insertExpr "insert into table1 values (5,'POPL',True)") mdb6
    let mdb8 = evaluateInsert (regularParse insertExpr "insert into table1 values (7,'Project',True)") mdb7
    --print(mdb4)
    let sel =  regularParse queryExpr "select c1,c2 from table1 where c3 order by -c1"
    let out = evaluateQuery sel mdb8
    print(out)

    -- sampleCommands
    -- print("Hello")
    -- let m = parseWithWSEof (valueExpr []) "a or (x>=y) or (z<=m)"
    -- print(m)
    -- let map0 = Map.empty
    -- let map1 = Map.insert "a" (BoolLit False) map0
    -- let map2 = Map.insert "x" (NumLit 1) map1
    -- let map3 = Map.insert "y" (NumLit 2) map2
    -- let map4 = Map.insert "z" (NumLit 2) map3
    -- let map5 = Map.insert "m" (NumLit 1) map4
    -- print (evaluate2 map5 m)
