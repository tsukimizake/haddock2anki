module Main where

import Parser
import Formatter
import System.Environment (getArgs)
import Data.Maybe
assert :: IO()
assert = putStrLn "pass url as an arg!"

run :: String -> String -> IO()
run outputpath uri = do
    maybeitems <- allItems uri
    items <- return $ fromMaybe [] maybeitems
    res <- return . name2doc $ items
    writeFile outputpath res

main :: IO()
main = do
     args <- getArgs
     if length args == 0
         then assert
         else run "./out.txt" (head args)
