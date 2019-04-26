module Main where

import Funcs
import Database
import SQLParser
import System.IO
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Exception(try,SomeException,catch)
import System.IO

-- | 'check' checks the type of response and prints it if possible. Otherwise, 
check :: ResType -> ResType -> IO()
check resp prev = case resp of 
    OUT arg -> do
        queryPrinter resp
        hFlush stdout
        func prev
    DB arg -> do
        if resp==prev then putStrLn "Database Unchanged!" else putStrLn "Done!"
        hFlush stdout
        func resp
    ERROR arg -> do
        putStrLn ("Error : "++arg)
        hFlush stdout
        func prev

evaluate :: String -> ResType -> ResType
evaluate input resp = evaluateSQL (parseWithWSEof sqlExpr input) (resp)

prompt :: String -> IO String
prompt a = do
  putStr a
  hFlush stdout
  getLine

func :: ResType -> IO ()
func prev = do
    input <- prompt ">>> "
    case take 6 input of
        "exit" -> do
            putStrLn "Exiting."
        "output" -> do
            print(prev)
            func prev
        otherwise -> catch (check (evaluate input prev) prev) (handler prev)

handler :: ResType -> SomeException -> IO ()           
handler prev ex = (putStrLn $ "Caught exception: " ++ show ex) *> func prev

main :: IO ()
main = do
    putStrLn "--------------------------------------------------------------"
    putStrLn "HaSSQL version 1.0, Copyright IITH-SBJoshi - Team 8 (c) 2019  |"
    putStrLn "--------------------------------------------------------------"
    let prev = (DB (newDatabase "mdb1" Map.empty))
    func prev
