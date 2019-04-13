module Main where
import Test.HUnit as H
import ParserTests
import DatabaseTests
import IntegrationTests

main :: IO H.Counts
main = do
    putStrLn("\n\nParser Tests:\n")
    ParserTests.parserTests
    putStrLn("\nDatabase Tests:\n")
    DatabaseTests.databaseTests
    putStrLn("\nIntegration Tests:\n")
    IntegrationTests.integrationTests