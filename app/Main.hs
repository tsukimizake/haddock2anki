module Main where

import Scraper
import Formatter
import System.Environment (getArgs)
import Data.Maybe
import Text.HTML.Scalpel

assert :: IO()
assert = putStrLn "pass url as an arg!"

run :: String -> String -> IO()
run outputpath uri = do
    maybeitems <- scrapeURL uri items
    items <- return $ fromMaybe [] maybeitems
    res <- return . name2doc $ items
    writeFile outputpath res

main :: IO()
main = do
     args <- getArgs
     if length args == 0
         then assert
         else run "./out.txt" (head args)
