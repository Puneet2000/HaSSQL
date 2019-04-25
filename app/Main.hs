module Main where

import Funcs
import Database
import SQLParser
import System.IO
import Data.Map (Map)
import qualified Data.Map as Map
-- | 'check' checks the type of response and prints it if possible. Otherwise, 
check :: ResType -> ResType -> IO()
check resp prev = case resp of 
    OUT arg -> do
        queryPrinter resp
        hFlush stdout
    DB arg -> do
        putStrLn "Done!"
        hFlush stdout
    ERROR arg -> do
        putStrLn "Error!"
        hFlush stdout
        func prev

evaluate :: String -> ResType -> ResType
evaluate input resp = evaluateSQL (regularParse sqlExpr input) (resp)

prompt :: String -> IO String
prompt a = do
  putStr a
  hFlush stdout
  getLine

func :: ResType -> IO ()
-- main = print (regularParse createExpr "create table table1 ( c1 INTEGER , c2 INTEGER )")
func prev = do
    -- let mdb2 = evaluateSQL (regularParse sqlExpr "create table table1 ( c1 INTEGER , c2 STRING , c3 BOOL)") (DB mdb)
    -- let mdb3 = evaluateSQL (regularParse sqlExpr "insert into table1 values (1,'Puneet',True)") mdb2
    -- let mdb4 = evaluateSQL (regularParse sqlExpr "insert into table1 values (11,'Shraiysh',False)") mdb3
    -- let mdb5 = evaluateSQL (regularParse sqlExpr "insert into table1 values (1,'Sai ramana',True)") mdb4
    -- let mdb6 = evaluateSQL (regularParse sqlExpr "insert into table1 values (2,'Hitesh',False)") mdb5
    -- let mdb7 = evaluateSQL (regularParse sqlExpr "insert into table1 values (5,'POPL',True)") mdb6
    -- let mdb8 = evaluateSQL (regularParse sqlExpr "insert into table1 values (7,'Project',True)") mdb7
    -- print(mdb8)
    -- let sel =  regularParse sqlExpr "delete from table1 where True"
    -- let out = evaluateSQL sel mdb8
    -- print(out)
    input <- prompt ">>> "
    if take 6 input `elem` ["create", "select", "delete", "insert"]
        then do
            let output = evaluate input prev
            check output prev
            func output
        else
            case take 6 input of
                "exit" -> do
                    putStrLn "Exiting."
                "output" -> do
                    print(prev)
                    func prev
                otherwise -> do
                    putStrLn "Error! Invalid Input."
                    func prev
main :: IO ()
main = do
    let cr = regularParse sqlExpr "create table table1 ( c1 INTEGER , c2 STRING , c3 BOOL)"
    let prev = (DB (newDatabase "mdb1" Map.empty))
    func prev
