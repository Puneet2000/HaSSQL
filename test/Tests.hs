module Main where
import Test.HUnit as H
import ParserTests
import DatabaseTests

main :: IO H.Counts
main = do
    putStrLn("\nParser Tests:\n")
    ParserTests.parserTests
    putStrLn("\nDatabase Tests:\n")
    DatabaseTests.databaseTests