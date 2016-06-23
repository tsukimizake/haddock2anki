module Main where

import Scraper
import Formatter
import System.Environment (getArgs)
import Data.Maybe
import Text.HTML.Scalpel
import Control.Monad (when)
import Data.List
import System.Console.ParseArgs

run :: String -> String -> IO()
run outputpath uri = do
    putStrLn "Scraping..."
    maybeitems <- scrapeURL uri items
    items <- return $ fromMaybe [] maybeitems
    res <- return . name2doc $ items
    writeFile outputpath res
    putStrLn $ "Done. Output is in " ++ outputpath

options = [Arg "outfile" (Just 'o') (Just "out") (argDataDefaulted "output file path" ArgtypeString "./out.txt") "output file.",
           Arg "url" Nothing Nothing (argDataRequired "url" ArgtypeString) "URL to parse"]

main :: IO()
main = do
     a <- parseArgsIO ArgsComplete options
     let url = fromJust $ getArg a "url"
     let outfile = fromJust $ getArg a "outfile"
     run outfile url
